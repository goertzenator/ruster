use super::*;

use std::mem::uninitialized;
use std::slice::{from_raw_parts, from_raw_parts_mut};



impl<'a, 'b, E: Env> TryEnvFrom<ScopedTerm<'a>, E> for &'a [u8] {
    type Err = Error;
    fn try_efrom(term: ScopedTerm<'a>, env: &E) -> Result<Self> {
        unsafe {
            let mut binary = uninitialized();
            match ens::enif_inspect_binary(env.into_ptr(), term.into(), &mut binary) {
                0 => Err(Error::Badarg),
                _ => Ok(from_raw_parts(binary.data, binary.size)),
            }
        }
    }
}

#[repr(C)]
pub struct Binary(ens::ErlNifBinary);


impl Binary {
    pub fn new(size: usize) -> Self {
        unsafe {
            let mut bin = uninitialized();
            match ens::enif_alloc_binary(size, &mut bin) {
                0 => panic!("enif_alloc_binary() failed"),
                _ => Binary(bin),
            }
        }
    }

    fn into_ptr(&self) -> *mut erlang_nif_sys::ErlNifBinary {
        &(self.0) as *const erlang_nif_sys::ErlNifBinary as *mut erlang_nif_sys::ErlNifBinary
    }
}

impl AsRef<[u8]> for Binary {
    fn as_ref(&self) -> &[u8] {
        unsafe { from_raw_parts(self.0.data, self.0.size) }
    }
}
impl AsMut<[u8]> for Binary {
    fn as_mut(&mut self) -> &mut [u8] {
        unsafe { from_raw_parts_mut(self.0.data, self.0.size) }
    }
}

impl Drop for Binary {
    fn drop(&mut self) {
        unsafe { ens::enif_release_binary(&mut self.0) }
    }
}


impl<'b, E: Env> EnvFrom<Binary, E> for ScopedTerm<'b> {
    fn efrom(bin: Binary, env: &E) -> Self {
        unsafe { ens::enif_make_binary(env.into_ptr(), bin.into_ptr()).into() }
        // The Binary gets Dropped.
    }
}
