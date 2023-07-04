// shelf and self are indeed similar, but I'm not changing the name.
#![allow(clippy::similar_names)]
use std::fmt::Debug;

pub use figures::{Point, Rect, Size, UPixels};

#[derive(Debug)]
pub struct ShelfPacker {
    size: Size<UPixels>,
    allocated_width: u32,
    minimum_column_width: u16,
    columns: Vec<Column>,
}

impl ShelfPacker {
    pub const fn new(size: Size<UPixels>, minimum_column_width: u16) -> Self {
        Self {
            size,
            allocated_width: 0,
            minimum_column_width,
            columns: Vec::new(),
        }
    }

    pub fn allocate(&mut self, area: Size<UPixels>) -> Option<Allocation> {
        self.allocate_area(Size {
            width: area.width.0.try_into().expect("area allocated too large"),
            height: area.height.0.try_into().expect("area allocated too large"),
        })
        .map(|allocation| Allocation {
            id: allocation,
            rect: Rect::new(self.allocation_origin(allocation), area),
        })
    }

    fn allocate_area(&mut self, area: Size<u16>) -> Option<AllocationId> {
        for (column_index, column) in self
            .columns
            .iter_mut()
            .enumerate()
            .filter(|(_, col)| area.width <= col.width)
        {
            if let Some(allocation) = column.allocate(area, column_index) {
                return Some(allocation);
            }
        }

        // No shelves found in any column. Allocate a shelf.
        for (column_index, column) in self
            .columns
            .iter_mut()
            .enumerate()
            .filter(|(_, col)| area.width <= col.width)
        {
            let remaining_height = self.size.height - column.allocated_height;
            if remaining_height.0 >= u32::from(area.height) {
                return Some(column.allocate_in_new_shelf(area, column_index));
            } /*else if let Some(last_shelf) = column.shelves.last_mut() {
                  if last_shelf.remaining_width >= u32::from(area.width) {
                      let growable_height = remaining_height + last_shelf.height;
                      if growable_height >= u32::from(area.height) {
                          // We can grow the existing shelf to occupy the remaining area.
                      }
                  }
              }*/
        }

        let width = self.minimum_column_width.max(area.width);
        let remaining_width = self.size.width - self.allocated_width;
        if u32::from(width) <= remaining_width.0 {
            let mut column = Column::new(self.allocated_width, width);
            self.allocated_width += u32::from(width);
            let allocation = column.allocate_in_new_shelf(area, self.columns.len());
            self.columns.push(column);
            return Some(allocation);
        }

        None
    }

    fn allocation_origin(&self, allocation: AllocationId) -> Point<UPixels> {
        let col = &self.columns[usize::from(allocation.column)];
        let shelf = &col.shelves[usize::from(allocation.shelf)];
        Point::new(col.x + u32::from(allocation.offset), shelf.y)
    }

    pub fn free(&mut self, id: AllocationId) {
        let column = &mut self.columns[usize::from(id.column)];
        let shelf = &mut column.shelves[usize::from(id.shelf)];
        let Some((mut index, allocation)) = shelf.allocations.iter_mut().enumerate().find(|(_, allocation)| allocation.offset() == id.offset) else { unreachable!("can't find allocation to free") };
        assert!(allocation.allocated());
        allocation.deallocate();
        let length = allocation.length;

        // If either the previous or next allocations are free, merge.
        let mut removed = 0;
        if index > 0 && !shelf.allocations[index - 1].allocated() {
            removed += 1;
            shelf.allocations.remove(index);
            index -= 1;
            shelf.allocations[index].length += length;
        }

        // Now check if the next can be merged
        let next = index + 1;
        if next < shelf.allocations.len() && !shelf.allocations[next].allocated() {
            let length = shelf.allocations[next].length;
            removed += 1;
            shelf.allocations.remove(next);
            shelf.allocations[index].length += length;
        }

        let newly_freed_index = index.try_into().expect("too many allocations");
        if removed > 0 {
            // The removal was merged. Update the free list, accounting for
            // removals.
            let removed_range = newly_freed_index + 1..=newly_freed_index + removed;
            shelf.free_list.retain_mut(|free_index| {
                if removed_range.contains(free_index) {
                    false
                } else {
                    if *free_index > newly_freed_index {
                        *free_index -= removed;
                    }
                    true
                }
            });
        } else {
            // Insert into the free list, maintaining order.
            let mut inserted = false;
            for (free_list_index, free_index) in shelf.free_list.iter().enumerate() {
                if *free_index > newly_freed_index {
                    shelf.free_list.insert(free_list_index, newly_freed_index);
                    inserted = true;
                    break;
                }
            }
            if !inserted {
                shelf.free_list.push(newly_freed_index);
            }
        }
    }

    pub fn allocated(&self) -> UPixels {
        let mut allocated = 0;

        for col in &self.columns {
            for shelf in &col.shelves {
                for allocation in &shelf.allocations {
                    if allocation.allocated() {
                        allocated += u32::from(allocation.length) * u32::from(shelf.height);
                    }
                }
            }
        }

        allocated.into()
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Allocation {
    pub id: AllocationId,
    pub rect: Rect<UPixels>,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct AllocationId {
    shelf: u16,
    column: u16,
    offset: u16,
}

#[derive(Debug)]
struct Column {
    x: u32,
    allocated_height: u32,
    width: u16,
    shelves: Vec<Shelf>,
    shelves_by_height: Vec<u16>,
}

impl Column {
    pub const fn new(x: u32, width: u16) -> Self {
        Self {
            x,
            width,
            shelves: Vec::new(),
            shelves_by_height: Vec::new(),
            allocated_height: 0,
        }
    }

    pub fn new_shelf(&mut self, height: u16) -> &mut Shelf {
        let shelf = Shelf::new(self.allocated_height, height);
        self.allocated_height += u32::from(height);
        let index = self.shelves.len();
        self.shelves.push(shelf);
        &mut self.shelves[index]
    }

    pub fn allocate_in_new_shelf(&mut self, area: Size<u16>, my_index: usize) -> AllocationId {
        let column_width = self.width;
        let shelf_index = self.shelves.len().try_into().expect("too many shelves");
        let shelf = self.new_shelf(area.height);
        let offset = shelf
            .allocate(area.width, column_width)
            .expect("new shelf must have enough free space");
        let by_height_index = self
            .shelves_by_height
            .binary_search_by(|index| self.shelves[usize::from(*index)].height.cmp(&area.height))
            .map_or_else(|i| i, |i| i);
        self.shelves_by_height.insert(by_height_index, shelf_index);
        AllocationId {
            shelf: shelf_index,
            column: my_index.try_into().expect("too many columns"),
            offset,
        }
    }

    pub fn allocate(&mut self, area: Size<u16>, my_index: usize) -> Option<AllocationId> {
        // Iterate over the shelves in order of their height.
        for shelf_index in &self.shelves_by_height {
            let shelf = &mut self.shelves[usize::from(*shelf_index)];
            if shelf.height < area.height {
                continue;
            }

            if shelf.height / 2 > area.height {
                // We want to avoid allocating into a shelf when we leave over
                // 50% of the space empty.
                break;
                // TODO we should keep track of a fallback allocation for when
                // the texture is packed so much that an unoptimal allocation is
                // better than failing to allocate.
            }

            if let Some(offset) = shelf.allocate(area.width, self.width) {
                return Some(AllocationId {
                    shelf: *shelf_index,
                    column: my_index.try_into().expect("too many columns"),
                    offset,
                });
            }
        }

        None
    }
}

#[derive(Debug)]
struct Shelf {
    y: u32,
    height: u16,
    allocated_width: u16,
    allocations: Vec<Slot>,
    free_list: Vec<u16>,
}

impl Shelf {
    pub const fn new(y: u32, height: u16) -> Self {
        Self {
            y,
            height,
            allocated_width: 0,
            allocations: Vec::new(),
            free_list: Vec::new(),
        }
    }

    pub fn allocate(&mut self, width: u16, column_width: u16) -> Option<u16> {
        // Reuse space to begin with
        for (free_list_index, free_index) in self.free_list.iter().copied().enumerate() {
            let allocation = &mut self.allocations[usize::from(free_index)];
            if let Some(extra_space) = allocation.length.checked_sub(width) {
                let offset = allocation.offset();
                allocation.allocate();
                if extra_space > 0 {
                    // Add a new allocation to keep track of the freed space.
                    let new_offset = offset + width;
                    self.allocations[usize::from(free_index)].length = width;
                    self.allocations.insert(
                        usize::from(free_index + 1),
                        Slot::new_free(new_offset, extra_space),
                    );
                    for free in self.free_list.iter_mut() {
                        if *free >= free_index {
                            *free += 1;
                        }
                    }
                } else {
                    // The allocation was fully used, remove it from the free
                    // list.
                    self.free_list.remove(free_list_index);
                }
                return Some(offset);
            }
        }

        // Allocate in the remaining space if nothing exists in the free list
        // that works.
        let remaining_width = column_width - self.allocated_width;
        if remaining_width >= width {
            let offset = self.allocated_width;
            self.allocations.push(Slot::new(offset, width));
            self.allocated_width += width;

            Some(offset)
        } else {
            None
        }
    }
}

struct Slot {
    status: u16,
    length: u16,
}

impl Debug for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Allocation")
            .field("allocated", &self.allocated())
            .field("offset", &self.offset())
            .field("length", &self.length)
            .finish()
    }
}

impl Slot {
    const ALLOCATED_BIT: u16 = 0x8000;
    const OFFSET_MASK: u16 = 0x7FFF;

    const fn allocated(&self) -> bool {
        self.status & Self::ALLOCATED_BIT != 0
    }

    const fn offset(&self) -> u16 {
        self.status & Self::OFFSET_MASK
    }

    pub fn new(offset: u16, length: u16) -> Self {
        let mut allocation = Slot::new_free(offset, length);
        allocation.allocate();
        allocation
    }

    pub fn new_free(offset: u16, length: u16) -> Self {
        let allocation = Self {
            status: offset,
            length,
        };
        assert!(!allocation.allocated(), "length too large");
        allocation
    }

    fn allocate(&mut self) {
        self.status |= Self::ALLOCATED_BIT;
    }

    fn deallocate(&mut self) {
        self.status &= Self::OFFSET_MASK;
    }
}

#[test]
fn basics() {
    let mut packer = ShelfPacker::new(Size::new(32, 2), 16);
    let first = dbg!(packer.allocate(Size::new(8, 1)).unwrap());
    let second = dbg!(packer.allocate(Size::new(8, 1)).unwrap());
    assert_eq!(first.id.column, second.id.column);
    assert_eq!(first.id.shelf, second.id.shelf);
    assert_ne!(first.id.offset, second.id.offset);
    assert!(!first.rect.intersects(&second.rect));
    packer.free(first.id);
    let reallocated = dbg!(packer.allocate(Size::new(8, 1)).unwrap());
    assert_eq!(first, reallocated);

    // New allocations will overflow into the next shelf
    let overflowed = dbg!(packer.allocate(Size::new(8, 1)).unwrap());
    assert_eq!(first.id.column, overflowed.id.column);
    assert_ne!(first.id.shelf, overflowed.id.shelf);

    // We are leaving an empty 8 pixels on this location.

    let next_column = packer.allocate(Size::new(8, 2)).unwrap();
    assert_ne!(first.id.column, next_column.id.column);

    let last_allocation = packer.allocate(Size::new(8, 2)).unwrap();
    assert_eq!(last_allocation.id.column, next_column.id.column);

    assert_eq!(packer.allocated().0, 56);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn space_reuse() {
        let mut packer = ShelfPacker::new(Size::new(16, 1), 16);
        // Fill
        let a = packer.allocate(Size::new(6, 1)).unwrap();
        let b = packer.allocate(Size::new(6, 1)).unwrap();
        let c = packer.allocate(Size::new(4, 1)).unwrap();
        assert!(packer.allocate(Size::new(1, 1)).is_none());

        // Free b, then allocate two chunks from it
        packer.free(b.id);
        let d = packer.allocate(Size::new(3, 1)).unwrap();
        let e = packer.allocate(Size::new(3, 1)).unwrap();
        assert!(packer.allocate(Size::new(1, 1)).is_none());
        // Free d and e and reallocate a full 6 again
        packer.free(d.id);
        packer.free(e.id);
        let f = packer.allocate(Size::new(6, 1)).unwrap();

        // Now free the entire structure
        packer.free(a.id);
        packer.free(c.id);
        packer.free(f.id);
        assert_eq!(packer.columns[0].shelves[0].free_list, &[0]);
        assert!(!packer.columns[0].shelves[0].allocations[0].allocated());
        assert_eq!(packer.columns[0].shelves[0].allocations[0].offset(), 0);
        assert_eq!(packer.columns[0].shelves[0].allocations[0].length, 16);
    }

    #[derive(Debug)]
    enum Op {
        Allocate(u8, u8),
        Free(usize),
    }

    fn run_ops(ops: Vec<Op>, width: u32, height: u32, column_size: u16) {
        let mut allocations = Vec::new();

        let mut rects = ShelfPacker::new(Size::new(width, height), column_size);
        for op in ops {
            match op {
                Op::Allocate(width, height) => {
                    if let Some(allocation) =
                        rects.allocate(Size::new(u32::from(width), u32::from(height)))
                    {
                        println!("Allocated {width}x{height}");
                        allocations.push(allocation);
                    }
                }
                Op::Free(index) => {
                    if !allocations.is_empty() {
                        let index = index % allocations.len();
                        println!("Freeing {index}");
                        let allocation = allocations.remove(index);
                        rects.free(allocation.id);
                    }
                }
            }
            println!("{rects:#?}");
        }
    }

    #[test]
    fn split_with_free_list() {
        // Verify the free list is properly maintained when splitting an
        // allocation
        run_ops(
            vec![
                Op::Allocate(143, 64),
                Op::Allocate(62, 64),
                Op::Free(0),
                Op::Allocate(62, 64),
                Op::Free(0),
            ],
            1024,
            1024,
            256,
        );
    }

    #[test]
    fn split_large_free_many_times() {
        run_ops(
            vec![
                Op::Allocate(166, 2),
                Op::Free(0),
                Op::Allocate(9, 2),
                Op::Allocate(9, 2),
                Op::Allocate(9, 2),
                Op::Free(0),
                Op::Free(0),
                Op::Allocate(9, 2),
                Op::Free(0),
            ],
            1024,
            1024,
            256,
        )
    }
}
