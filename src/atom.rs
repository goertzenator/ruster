use super::*;

////////////
// Atom

#[derive(PartialEq, Eq)]
pub struct Atom<T:AsRef<str>>(T);

impl<'a, T:AsRef<str>> Bind for Atom<T> {}

impl<'a, T:AsRef<str>> AsRef<str> for Atom<T> {
	fn as_ref(&self) -> &str {
		self.0.as_ref()
	}
}

impl<'b, T:AsRef<str>> From<Binder<'b, Atom<T>>> for StaticTerm {
	fn from(b: Binder<'b, Atom<T>>) -> Self {
		let name: &str = b.val.as_ref();
		StaticTerm::new( unsafe{ ens::enif_make_atom_len(b.env.as_api_ptr(), name.as_ptr(), name.len()) } )
	}
}

impl<'a, 'b, T> TryFrom<Binder<'b, T>> for Atom<String>
	where T: Into<CTerm> + Copy
{
	type Err = Error;
	fn try_from(b: Binder<'b, T>) -> Result<Self> {
		unsafe {

			// get atom length
			let mut length = std::mem::uninitialized();
			if 0 != ens::enif_get_atom_length(b.env.as_api_ptr(), b.val.into(), &mut length, ens::ErlNifCharEncoding::ERL_NIF_LATIN1) {
				return Err(Error::Badarg);
			}

			// get atom as Vec
			let mut v = Vec::with_capacity(length as usize);
			v.set_len(length as usize);
			if 0 != ens::enif_get_atom(
				b.env.as_api_ptr(),
				b.val.into(),
				v.as_mut_ptr(),
				length,
				ens::ErlNifCharEncoding::ERL_NIF_LATIN1) {

				return Err(Error::Badarg);
			}

			// convert to String
			Ok(Atom(String::from_utf8_unchecked(v)))  // trust that enif_get_atom enforced encoding
			// String::from_utf8(v)
			// 	.map_err(|_|Error::Badarg)
			// 	.map(|s|Atom(s))
		}
	}
}






////////////
// StaticAtom

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct StaticAtom(pub usize);  // pub so user can construct

impl Bind for StaticAtom {}

static mut STATIC_ATOM_DATA: *mut StaticAtomData = 0 as *mut StaticAtomData;

pub fn init_static_atom_data(env: &Env, atom_names: &[AtomInit]) {
	let r = Box::new( StaticAtomData::new(env, atom_names) );
	unsafe {
		STATIC_ATOM_DATA = Box::into_raw(r);
	}
}

pub fn destroy_static_atom_data() {
	unsafe {
		Box::from_raw(STATIC_ATOM_DATA);
		STATIC_ATOM_DATA = std::ptr::null_mut();
	}
}


struct StaticAtomData {
	terms: Vec<CTerm>,     // get CTerm for given StaticAtom.  Also used for reverse lookups
	                       // Reverse lookup is linear search which is probably fastest for <100-ish atoms.
	                       // If someone has a larger use case then a separate structure for binary
	                       // search or BTree may be in order.
	names: Vec<String>,    // get name given StaticAtom
}

impl StaticAtomData {
	fn new(env: &Env, atom_names: &[AtomInit]) -> StaticAtomData {
		let names: Vec<String> = atom_names.iter().map(|x| x.to_string()).collect();
		let terms: Vec<CTerm> = names.iter().map(|name| {
				let a = Atom(name);
				let t:StaticTerm = a.bind(env).into();
				t.into()
			}).collect();

		StaticAtomData{ terms: terms, names:names }
	}
}

pub enum AtomInit<'a> {
	Lowercase(&'a str),
	AsIs(&'a str),
}

impl<'a> AtomInit<'a> {
	fn to_string(&self) -> String {
		match *self {
			AtomInit::Lowercase(s) => s.to_lowercase(),
			AtomInit::AsIs(s)      => s.to_string(),
		}
	}
}

// StaticAtom/Term conversions, also binderless conversions


impl From<StaticAtom> for StaticTerm {
	fn from(sa: StaticAtom) -> Self {
		unsafe {
			StaticTerm::new((*STATIC_ATOM_DATA).terms[sa.0])
		}
	}
}

impl<'a, T> TryFrom<T> for StaticAtom
	where T: Into<CTerm>
{
	type Err = Error;
	fn try_from(t: T) -> Result<Self> {
		unsafe {
			let cterm: CTerm = t.into();
			(*STATIC_ATOM_DATA).terms.iter().cloned().position(|x|x==cterm).map(|x|StaticAtom(x)).ok_or(Error::Badarg)
			//(*STATIC_ATOM_DATA).reverse_terms.get(t).cloned().ok_or(Error::Badarg)
		}
	}
}

impl<'e> From<Binder<'e, StaticAtom>> for StaticTerm {
	fn from(b: Binder<'e, StaticAtom>) -> StaticTerm {
		From::from(b.val)
	}
}

impl<'e> From<Binder<'e, StaticAtom>> for ScopedTerm<'e> {
	fn from(b: Binder<'e, StaticAtom>) -> ScopedTerm<'e> {
		unsafe {
			ScopedTerm::new((*STATIC_ATOM_DATA).terms[b.val.0])
		}
	}
}



impl<'a, 'b, T> TryFrom<Binder<'b, T>> for StaticAtom
	where T: Into<CTerm>
{
	type Err = Error;
	fn try_from(b: Binder<'b, T>) -> Result<Self> {
		TryFrom::try_from(b.val)
	}
}

// StaticAtom/String conversions

impl AsRef<str> for StaticAtom {
	fn as_ref(&self) -> &'static str {
		unsafe{
			(*STATIC_ATOM_DATA).names[self.0].as_ref()
		}
	}
}

// StaticAtom/Atom conversions


// pub mod atom {

//     pub struct StaticAtom(usize);
//     impl Bind for StaticAtom {}

//     use std;
//     use ruster::*;
//     pub const ok: StaticAtom = StaticAtom(0);
//     pub const error: StaticAtom = StaticAtom(1);
//     pub const add: StaticAtom = StaticAtom(2);
//     pub const sub: StaticAtom = StaticAtom(3);
//     pub const mul: StaticAtom = StaticAtom(4);

//     static mut STATIC_ATOMS: [Term<'static>;5] = [UNINITIALIZED_TERM; 5];

//     pub fn init(env: &Env) {
//         unsafe STATIC_ATOMS = [
//             Atom::new("ok"),
//             Atom::new("error"),
//             Atom::new("add"),
//             Atom::new("sub"),
//             Atom::new("mul"),
//         ];

//     }

//     impl From<StaticAtom> for Term<'static> {
//         fn from(s: StaticAtom) -> Self {
//             unsafe{ STATIC_ATOMS[s.0] }
//         }
//     }
// }
