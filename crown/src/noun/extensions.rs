use sword::mem::NockStack;

use std::ffi::CStr;
use crate::{Noun, Result, ToBytes, ToBytesExt};
use bincode::{Decode, Encode};
use bytes::Bytes;
use std::iter::Iterator;
use sword::noun::{Atom, IndirectAtom, D};
use sword::serialization::{cue, jam};

pub trait NounExt {
    fn cue_bytes(stack: &mut NockStack, bytes: &Bytes) -> Noun;
    fn cue_bytes_slice(stack: &mut NockStack, bytes: &[u8]) -> Noun;
    fn jam_self(self, stack: &mut NockStack) -> JammedNoun;
    fn list_iter(self) -> impl Iterator<Item = Noun>;
    fn eq_bytes(self, bytes: impl AsRef<[u8]>) -> bool;
}

impl NounExt for Noun {
    fn cue_bytes(stack: &mut NockStack, bytes: &Bytes) -> Noun {
        let atom = Atom::from_bytes(stack, bytes);
        cue(stack, atom)
    }

    // generally, we should be using `cue_bytes`, but if we're not going to be passing it around
    // its OK to just cue a byte slice to avoid copying.
    fn cue_bytes_slice(stack: &mut NockStack, bytes: &[u8]) -> Noun {
        let atom = unsafe {
            IndirectAtom::new_raw_bytes(stack, bytes.len(), bytes.as_ptr()).normalize_as_atom()
        };
        cue(stack, atom)
    }

    fn jam_self(self, stack: &mut NockStack) -> JammedNoun {
        JammedNoun::from_noun(stack, self)
    }

    fn list_iter(self) -> impl Iterator<Item = Noun> {
        NounListIterator(self)
    }

    fn eq_bytes(self, bytes: impl AsRef<[u8]>) -> bool {
        if let Ok(a) = self.as_atom() {
            a.eq_bytes(bytes)
        } else {
            false
        }
    }
}

pub trait AtomExt {
    fn from_bytes(stack: &mut NockStack, bytes: &Bytes) -> Atom;
    fn from_value<T: ToBytes>(stack: &mut NockStack, value: T) -> Result<Atom>;
    fn eq_bytes(self, bytes: impl AsRef<[u8]>) -> bool;
    fn to_bytes_until_nul(self) -> Option<Vec<u8>>;
}

impl AtomExt for Atom {
    fn from_bytes(stack: &mut NockStack, bytes: &Bytes) -> Atom {
        unsafe {
            IndirectAtom::new_raw_bytes(stack, bytes.len(), bytes.as_ptr()).normalize_as_atom()
        }
    }

    fn from_value<T: ToBytes>(stack: &mut NockStack, value: T) -> Result<Atom> {
        unsafe {
            let data: Bytes = value.as_bytes()?;
            Ok(IndirectAtom::new_raw_bytes(stack, data.len(), data.as_ptr()).normalize_as_atom())
        }
    }

    /** Test for byte equality, ignoring trailing 0s in the Atom representation
        beyond the length of the bytes compared to
    */
    fn eq_bytes(self, bytes: impl AsRef<[u8]>) -> bool {
        let bytes_ref = bytes.as_ref();
        let atom_bytes = self.as_bytes();
        if bytes_ref.len() > atom_bytes.len() {
            false
        } else if bytes_ref.len() == atom_bytes.len() {
            atom_bytes == bytes_ref
        } else {
            // check for nul bytes beyond comparing bytestring
            for b in &atom_bytes[bytes_ref.len()..] {
                if *b != 0u8 {
                    return false;
                }
            }
            &atom_bytes[0..bytes_ref.len()] == bytes_ref
        }
    }

    fn to_bytes_until_nul(self) -> Option<Vec<u8>>   {
        if let Ok(cstr) = CStr::from_bytes_until_nul(self.as_bytes()) {
            Some(cstr.to_bytes().to_vec())
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Debug, Encode, Decode)]
pub struct JammedNoun(#[bincode(with_serde)] pub Bytes);

impl JammedNoun {
    pub fn new(bytes: Bytes) -> Self {
        Self(bytes)
    }

    pub fn from_noun(stack: &mut NockStack, noun: Noun) -> Self {
        let jammed_atom = jam(stack, noun);
        JammedNoun(Bytes::copy_from_slice(jammed_atom.as_bytes()))
    }

    pub fn cue_self(&self, stack: &mut NockStack) -> Noun {
        let atom = unsafe {
            IndirectAtom::new_raw_bytes(stack, self.0.len(), self.0.as_ptr()).normalize_as_atom()
        };
        cue(stack, atom)
    }
}

impl From<&[u8]> for JammedNoun {
    fn from(bytes: &[u8]) -> Self {
        JammedNoun::new(Bytes::copy_from_slice(bytes))
    }
}

impl From<Vec<u8>> for JammedNoun {
    fn from(byte_vec: Vec<u8>) -> Self {
        JammedNoun::new(Bytes::from(byte_vec))
    }
}

impl AsRef<Bytes> for JammedNoun {
    fn as_ref(&self) -> &Bytes {
        &self.0
    }
}

impl AsRef<[u8]> for JammedNoun {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl Default for JammedNoun {
    fn default() -> Self {
        JammedNoun::new(Bytes::new())
    }
}

pub struct NounListIterator(Noun);

impl Iterator for NounListIterator {
    type Item = Noun;
    fn next(&mut self) -> Option<Self::Item> {
        if let Ok(it) = self.0.as_cell() {
            self.0 = it.tail();
            Some(it.head())
        } else if unsafe { self.0.raw_equals(D(0)) } {
            None
        } else {
            panic!("Improper list terminator: {:?}", self.0)
        }
    }
}
