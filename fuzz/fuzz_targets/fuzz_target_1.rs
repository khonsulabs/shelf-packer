#![no_main]

use std::num::{NonZeroU16, NonZeroU8};

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use shelf_packer::{Size, TexturePacker, UPixels};

#[derive(Debug, Arbitrary)]
enum Op {
    Allocate(NonZeroU8, Height),
    Free(usize),
}

#[derive(Debug)]
struct Height(u16);
impl<'a> Arbitrary<'a> for Height {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let height_power = u.arbitrary::<u8>()? % 8;
        let height = 2_u16.pow(u32::from(height_power));
        Ok(Height(height))
    }
}

fuzz_target!(|program: Vec<Op>| {
    let mut allocations = Vec::new();

    let mut rects = TexturePacker::new(Size::new(1024, 1024), 256);
    for op in program {
        match op {
            Op::Allocate(width, height) => {
                if let Some(allocation) =
                    rects.allocate(Size::new(u32::from(width.get()), u32::from(height.0)))
                {
                    allocations.push(allocation);
                }
            }
            Op::Free(index) => {
                if !allocations.is_empty() {
                    let index = index % allocations.len();
                    let allocation = allocations.remove(index);
                    rects.free(allocation.allocation);
                }
            }
        }
    }
});
