use super::*;

use std::mem::uninitialized;
use std::slice::{from_raw_parts, from_raw_parts_mut};

impl<'a> FromTerm<CTerm> for &'a[u8] {
	fn from_term(env: &mut Env, term: CTerm) -> Result<Self> {
		unsafe {
			let mut binary = uninitialized();
			match ens::enif_inspect_binary(env, term, &mut binary) {
				0 => Err(Error::Badarg),
				_ => Ok(from_raw_parts(binary.data, binary.size))
			}
		}
	}
}

/// Creat a new binary Term and a mutable slice referring to its contents.
pub fn new_binary(env: &mut Env, size: usize) -> (Term, &mut [u8]) {
	unsafe {
		let mut cterm = uninitialized();
		let ptr = ens::enif_make_new_binary(env, size, &mut cterm);
		let term = cterm.as_term(env); // maybe wrap in checked term
		(term, from_raw_parts_mut(ptr,size))
	}
}
