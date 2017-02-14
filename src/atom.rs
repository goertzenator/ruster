use super::*;

////////////
// Atom

#[derive(PartialEq, Eq)]
pub struct Atom<T: AsRef<str>>(T);

impl<'a, T: AsRef<str>> Bind for Atom<T> {}

impl<'a, T: AsRef<str>> AsRef<str> for Atom<T> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}


impl<'e, E: Env, T: AsRef<str>> From<Binder<'e, E, Atom<T>>> for ScopedTerm<'e> {
    fn from(b: Binder<'e, E, Atom<T>>) -> Self {
        let name: &str = b.val.as_ref();
        ScopedTerm::new(unsafe {
            ens::enif_make_atom_len(b.env.as_api_ptr(), name.as_ptr(), name.len())
        })
    }
}

impl<'e, E: Env, T: AsRef<str>> From<Binder<'e, E, Atom<T>>> for StaticTerm {
    fn from(b: Binder<'e, E, Atom<T>>) -> Self {
        let name: &str = b.val.as_ref();
        StaticTerm::new(unsafe {
            ens::enif_make_atom_len(b.env.as_api_ptr(), name.as_ptr(), name.len())
        })
    }
}


impl<'a, 'b, E: Env, T> TryFrom<Binder<'b, E, T>> for Atom<String>
    where T: Into<CTerm> + Copy
{
    type Err = Error;
    fn try_from(b: Binder<'b, E, T>) -> Result<Self> {
        unsafe {

            // get atom length
            let mut length = std::mem::uninitialized();
            if 0 !=
               ens::enif_get_atom_length(b.env.as_api_ptr(),
                                         b.val.into(),
                                         &mut length,
                                         ens::ErlNifCharEncoding::ERL_NIF_LATIN1) {
                return Err(Error::Badarg);
            }

            // get atom as Vec
            let mut v = Vec::with_capacity(length as usize);
            v.set_len(length as usize);
            if 0 !=
               ens::enif_get_atom(b.env.as_api_ptr(),
                                  b.val.into(),
                                  v.as_mut_ptr(),
                                  length,
                                  ens::ErlNifCharEncoding::ERL_NIF_LATIN1) {

                return Err(Error::Badarg);
            }

            // convert to String
            Ok(Atom(String::from_utf8_unchecked(v))) // trust that enif_get_atom enforced encoding
            // String::from_utf8(v)
            // 	.map_err(|_|Error::Badarg)
            // 	.map(|s|Atom(s))
        }
    }
}

////////////
// StaticAtom

pub trait StaticAtom: Sized + PartialEq + Eq + Copy + Clone {
    fn atom_table() -> &'static mut [CTerm];
    fn name_table() -> &'static [AtomName<'static>];
    fn index(&self) -> usize;
    fn new(i: usize) -> Self;

    fn lookup(self) -> CTerm {
        Self::atom_table()[self.index()]
    }

    fn reverse_lookup(term: CTerm) -> Result<Self> {
        Self::atom_table()
            .iter()
            .cloned()
            .position(|x| x == term)
            .map(|x| Self::new(x))
            .ok_or(Error::Badarg)
    }

    fn init(penv: *mut ens::ErlNifEnv) {
        let mut atoms = Self::atom_table();
        let names = Self::name_table();
        assert_eq!(atoms.len(), names.len());
        for (atom, atominit) in atoms.iter_mut().zip(names) {
            let binder: Binder<ProcEnv, Atom<String>> = Atom(atominit.to_string()) . bind(ProcEnv::from_api_ptr(penv));
            let term = ScopedTerm::from(binder);
            //let term = StaticTerm::from(binder);
            *atom = term.into();
        }
    }
}

//impl<A: StaticAtom> Bind for A {}

// #[derive(PartialEq, Eq, Copy, Clone)]
// pub struct StaticAtom(pub usize); // pub so user can construct

// impl Bind for StaticAtom {}

// static mut STATIC_ATOM_DATA: *mut StaticAtomData = 0 as *mut StaticAtomData;

// pub fn init_static_atom_data(penv: *mut ens::ErlNifEnv, atom_names: &[AtomInit]) {
//     let r = Box::new(StaticAtomData::new(Env::from_api_ptr(penv), atom_names));
//     unsafe {
//         STATIC_ATOM_DATA = Box::into_raw(r);
//     }
// }

// pub fn destroy_static_atom_data() {
//     unsafe {
//         Box::from_raw(STATIC_ATOM_DATA);
//         STATIC_ATOM_DATA = std::ptr::null_mut();
//     }
// }


// struct StaticAtomData {
//     terms: Vec<CTerm>, // get CTerm for given StaticAtom.  Also used for reverse lookups
//     // Reverse lookup is linear search which is probably fastest for <100-ish atoms.
//     // If someone has a larger use case then a separate structure for binary
//     // search or BTree may be in order.
//     names: Vec<String>, // get name given StaticAtom
// }

// impl StaticAtomData {
//     fn new(env: &ProcEnv, atom_names: &[AtomInit]) -> StaticAtomData {
//         let names: Vec<String> = atom_names.iter().map(|x| x.to_string()).collect();
//         let terms: Vec<CTerm> = names.iter()
//             .map(|name| {
//                 let a = Atom(name);
//                 let t: StaticTerm = a.bind(env).into();
//                 t.into()
//             })
//             .collect();

//         StaticAtomData {
//             terms: terms,
//             names: names,
//         }
//     }
// }

pub enum AtomName<'a> {
    Lowercase(&'a str),
    AsIs(&'a str),
}

impl<'a> AtomName<'a> {
    fn to_string(&self) -> String {
        match *self {
            AtomName::Lowercase(s) => s.to_lowercase(),
            AtomName::AsIs(s) => s.to_string(),
        }
    }
}

// StaticAtom/Term conversions, also binderless conversions


impl<A: StaticAtom> From<A> for StaticTerm {
    fn from(sa: A) -> Self {
        StaticTerm::new(A::lookup(sa))
    }
}

impl<'a, T, A: StaticAtom> TryFrom<T> for A
    where T: Into<CTerm>
{
    type Err = Error;
    fn try_from(t: T) -> Result<Self> {
        A::reverse_lookup(t.into())
    }
}

impl<'e, E: Env, A: StaticAtom> From<Binder<'e, E, A>> for StaticTerm {
    fn from(b: Binder<'e, E, A>) -> StaticTerm {
        StaticTerm::new(A::lookup(b.val))
    }
}

impl<'e, E: Env, A: StaticAtom> From<Binder<'e, E, A>> for ScopedTerm<'e> {
    fn from(b: Binder<'e, E, A>) -> ScopedTerm<'e> {
        ScopedTerm::new(A::lookup(b.val))
    }
}



// This is too generic and conflicts with Resource conversion traits.
// Moving to macro for non-generic impls
impl<'e, E: Env, A: StaticAtom> TryFrom<Binder<'e, E, ScopedTerm<'e>>> for A
{
    type Err = Error;
    fn try_from(b: Binder<'e, E, ScopedTerm>) -> Result<Self> {
        TryFrom::try_from(b.val)
    }
}
impl<'e, E: Env, A: StaticAtom> TryFrom<Binder<'e, E, StaticTerm>> for A
{
    type Err = Error;
    fn try_from(b: Binder<'e, E, StaticTerm>) -> Result<Self> {
        TryFrom::try_from(b.val)
    }
}





// StaticAtom/String conversions

// impl AsRef<str> for StaticAtom {
//     fn as_ref(&self) -> &'static str {
//         unsafe { (*STATIC_ATOM_DATA).names[self.0].as_ref() }
//     }
// }



#[macro_export]
macro_rules! static_atom_pool {
    // strip trailing comma
    ($atomtype:ident, [$($atoms:tt),+,] ) => ( static_atom_pool!($atomtype, [$($atoms),*]) );

    ($atomtype:ident, [$($atoms:tt),*] ) => (
        #[derive(PartialEq, Eq, Copy, Clone)]
        pub struct $atomtype(usize);

        impl $crate::StaticAtom for $atomtype {
            fn atom_table() -> &'static mut [$crate::CTerm] {
                const CNT: usize = count_tts!($($atoms)*);
                static mut TABLE:[$crate::CTerm;CNT] = [0;CNT];
                unsafe {&mut TABLE}
            }
            fn name_table() -> &'static [AtomName<'static>] {
                const CNT: usize = count_tts!($($atoms)*);
                static TABLE:[$crate::AtomName<'static>;CNT] = [$(atom_name!($atoms)),*];
                &TABLE
            }
            fn index(&self) -> usize {
                self.0
            }
            fn new(i: usize) -> Self {
                $atomtype(i)
            }
        }

        decl_static_atoms!(0usize, $atomtype, [$($atoms),*]);

        impl<'e, E: Env> TryFrom<Binder<'e, E, ScopedTerm<'e>>> for $atomtype {
            type Err = Error;
            fn try_from(b: Binder<'e, E, ScopedTerm>) -> Result<Self> {
                TryFrom::try_from(b.val)
            }
        }
        impl<'e, E: Env> TryFrom<Binder<'e, E, StaticTerm>> for $atomtype {
            type Err = Error;
            fn try_from(b: Binder<'e, E, StaticTerm>) -> Result<Self> {
                TryFrom::try_from(b.val)
            }
        }

    );
}

#[macro_export]
macro_rules! atom_name {
    ( $id:ident )               => ( $crate::AtomName::Lowercase(stringify!($id)) );
    ( ($id:ident, $atom:expr) ) => ( $crate::AtomName::AsIs($atom) );
}

// ref https://danielkeep.github.io/tlborm/book/blk-counting.html
#[macro_export]
macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {$sub};
}
#[macro_export]
macro_rules! count_tts {
    ($($tts:tt)*) => {0usize $(+ replace_expr!($tts 1usize))*};
}


#[macro_export]
macro_rules! decl_static_atoms {
    ( $cnt:expr, $atomtype:ident, [ $atom:tt ] ) => (
        decl_static_atom!($atom, $atomtype, $cnt);
    );
    ( $cnt:expr, $atomtype:ident, [ $atom:tt, $($rest:tt),*] ) => (
        decl_static_atom!($atom, $atomtype, $cnt);
        decl_static_atoms!($cnt+1usize, $atomtype, [$($rest),*]);
    );
}

#[macro_export]
macro_rules! decl_static_atom {
    ( $id:ident, $atomtype:ident, $cnt:expr ) =>
        ( pub const $id: $atomtype = $atomtype($cnt); );
    ( ($id:ident, $atomtype:ident, $atom:expr, $cnt:expr) ) =>
        ( pub const $id: $atomtype = $atomtype($cnt); );
}

#[macro_export]
macro_rules! atom_name {
    ( $id:ident )               => ( $crate::AtomName::Lowercase(stringify!($id)) );
    ( ($id:ident, $atom:expr) ) => ( $crate::AtomName::AsIs($atom) );
}



static_atom_pool!(RusterStaticAtom, [OK,ERROR,UNDEFINED,TRUE,FALSE]);

