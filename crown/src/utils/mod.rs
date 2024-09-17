pub mod bytes;
pub mod error;
pub mod slogger;

use byteorder::{LittleEndian, ReadBytesExt};
pub use bytes::ToBytes;
use either::Either;
pub use error::{CrownError, Result};
use std::ptr::copy_nonoverlapping;
use std::slice::from_raw_parts_mut;
use std::time::{SystemTime, UNIX_EPOCH};
use sword::mem::NockStack;
use sword::noun::{Atom, IndirectAtom, Noun, NounAllocator};
use sword::serialization::jam;

// urbit @da timestamp
pub struct DA(pub u128);

// ~1970.1.1
const EPOCH_DA: u128 = 170141184475152167957503069145530368000;
// ~s1
const S1: u128 = 18446744073709551616;

pub const NOCK_STACK_1KB: usize = 1 << 7;

// nock stack size
pub const NOCK_STACK_SIZE: usize = (NOCK_STACK_1KB << 10 << 10) * 8; // 2GB

/**
 *   ::  +from-unix: unix seconds to @da
 *   ::
 *   ++  from-unix
 *     |=  timestamp=@ud
 *     ^-  @da
 *     %+  add  ~1970.1.1
 *     (mul timestamp ~s1)
 *   ::  +from-unix-ms: unix milliseconds to @da
 *   ::
 *   ++  from-unix-ms
 *     |=  timestamp=@ud
 *     ^-  @da
 *     %+  add  ~1970.1.1
 *     (div (mul ~s1 timestamp) 1.000)
*/

pub fn unix_ms_to_da(unix_ms: u128) -> DA {
    DA(EPOCH_DA + ((S1 * unix_ms) / 1000))
}

pub fn da_to_unix_ms(da: DA) -> u128 {
    ((da.0 - EPOCH_DA) * 1000) / S1
}

pub fn current_da() -> DA {
    let start = SystemTime::now();
    let since_the_epoch: u128 = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis();
    unix_ms_to_da(since_the_epoch)
}

pub fn current_epoch_ms() -> u128 {
    let start = SystemTime::now();
    start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis()
}

pub fn make_tas<A: NounAllocator>(allocator: &mut A, tas: &str) -> Atom {
    let tas_bytes: &[u8] = tas.as_bytes();
    unsafe {
        let mut tas_atom = IndirectAtom::new_raw_bytes(allocator, tas_bytes.len(), tas_bytes.as_ptr());
        tas_atom.normalize_as_atom()
    }
}

// serialize a noun for writing over a socket or a file descriptor
pub fn serialize_noun(stack: &mut NockStack, noun: Noun) -> Result<Vec<u8>> {
    let atom = jam(stack, noun);
    let size = atom.size() << 3;

    let buf = unsafe { from_raw_parts_mut(stack.struct_alloc::<u8>(size + 5), size + 5) };
    buf[0] = 0u8;
    buf[1] = size as u8;
    buf[2] = (size >> 8) as u8;
    buf[3] = (size >> 16) as u8;
    buf[4] = (size >> 24) as u8;

    match atom.as_either() {
        Either::Left(direct) => unsafe {
            copy_nonoverlapping(
                &direct.data() as *const u64 as *const u8,
                buf.as_mut_ptr().add(5),
                size,
            );
        },
        Either::Right(indirect) => unsafe {
            copy_nonoverlapping(
                indirect.data_pointer() as *const u8,
                buf.as_mut_ptr().add(5),
                size,
            );
        },
    };
    Ok(buf.to_vec())
}

pub fn compute_timer_time(time: Noun) -> Result<u64> {
    let time_atom = time.as_atom()?;
    let mut time_bytes: &[u8] = time_atom.as_bytes();
    let timer_time: u128 = da_to_unix_ms(DA(ReadBytesExt::read_u128::<LittleEndian>(
        &mut time_bytes,
    )?));
    let now: u128 = current_epoch_ms();
    let timer_ms: u64 = if now >= timer_time {
        1
    } else {
        (timer_time - now).try_into()?
    };
    Ok(timer_ms)
}
