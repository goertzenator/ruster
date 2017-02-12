
/// Implement exported module init function needed by the Erlang runtime.
///
/// See [the module level documentation](index.html) for usage of `nif_init!`.
///
#[macro_export]
macro_rules! ruster_init {
    ( $module:expr, $funcs_tt:tt) => ( ruster_init!($module, $funcs_tt, []));
    ( $module:expr, $funcs_tt:tt, $atoms_tt:tt) => ( ruster_init!($module, $funcs_tt, $atoms_tt, $crate::DummyPrivType) );
    ( $module:expr, $funcs_tt:tt, $atoms_tt:tt, $privtype:ty) => (

        nif_init!($module, $funcs_tt, {
            load: load,
            unload: $crate::unload::<$privtype>
        });

        fn load(penv: *mut $crate::erlang_nif_sys::ErlNifEnv,
                priv_data: *mut *mut $crate::erlang_nif_sys::c_void,
                load_info: $crate::erlang_nif_sys::ERL_NIF_TERM) -> $crate::erlang_nif_sys::c_int {
            $crate::init_static_atom_data(penv, atom::STATIC_ATOM_STRINGS);
            $crate::load::<MyType>(penv, priv_data, load_info)
        }

        #[allow(dead_code)]
        fn priv_data(env: & $crate::ProcEnv) -> &'static $privtype {
            $crate::internal_priv_data::<$privtype>(env)
        }

        impl_atoms_mod!($atoms_tt);
    )
}

// impl_atoms_mod! sample output:
// pub mod atom {
//     use std;
//     use ruster::*;
//     pub const OK: StaticAtom = StaticAtom(0);
//     pub const ERROR: StaticAtom = StaticAtom(1);
//     pub const ADD: StaticAtom = StaticAtom(2);
//     pub const SUB: StaticAtom = StaticAtom(3);
//     pub const MUL: StaticAtom = StaticAtom(4);
//     pub const DIVISION_BY_ZERO: StaticAtom = StaticAtom(5);
//     pub const DIV: StaticAtom = StaticAtom(6);
//
//     pub const static_atom_strings: &[AtomInit<'static>] = [
//         AtomInit::Lowercase("OK"),
//         AtomInit::Lowercase("ERROR"),
//         AtomInit::Lowercase("ADD"),
//         AtomInit::Lowercase("SUB"),
//         AtomInit::Lowercase("MUL"),
//         AtomInit::Lowercase("DIVISION_BY_ZERO"),
//         AtomInit::Lowercase("DIV"),
//     ];
// }
//
#[macro_export]
macro_rules! impl_atoms_mod {
    // strip trailing comma
    ( [$($atoms:tt),+,] ) => ( impl_atoms_mod!([$($atoms),*]) );

    ( [$($atoms:tt),*] ) => (

        pub mod atom {
            use $crate::*;

            decl_static_atoms!(0, [$($atoms),*]);

            pub const STATIC_ATOM_STRINGS: &'static [AtomInit<'static>] = &[$(atom_name!($atoms)),*];
        }
    );
}




#[macro_export]
macro_rules! decl_static_atoms {
    ( $cnt:expr, [ $atom:tt ] ) => (
        decl_static_atom!($atom, $cnt);
    );
    ( $cnt:expr, [ $atom:tt, $($rest:tt),*] ) => (
        decl_static_atom!($atom, $cnt);
        decl_static_atoms!($cnt+1, [$($rest),*]);
    );
}

#[macro_export]
macro_rules! decl_static_atom {
    ( $id:ident, $cnt:expr )               => ( pub const $id: StaticAtom = StaticAtom($cnt); );
    ( ($id:ident, $atom:expr, $cnt:expr) ) => ( pub const $id: StaticAtom = StaticAtom($cnt); );
}

#[macro_export]
macro_rules! atom_name {
    ( $id:ident )               => ( $crate::AtomInit::Lowercase(stringify!($id)) );
    ( ($id:ident, $atom:expr) ) => ( $crate::AtomInit::AsIs($atom) );
}



// #[cfg(test)]
// mod rustermacro_namespace_tests {

//     // explicitly disable for this test:
//     // use erlang_nif_sys_api::*;
//     use erlang_nif_sys_api;

//     use std;
//     use std::ptr;
//     use std::slice;
//     use std::ffi::{CString, CStr};

//     // Initializer tests
//     fn load(_env: *mut erlang_nif_sys_api::ErlNifEnv, _priv_data: *mut *mut erlang_nif_sys_api::c_void, _load_info: erlang_nif_sys_api::ERL_NIF_TERM) -> erlang_nif_sys_api::c_int {
//         14
//     }

//     fn unload(_env: *mut erlang_nif_sys_api::ErlNifEnv, _priv_data: *mut erlang_nif_sys_api::c_void) {}


//     fn raw_nif1(_env: *mut erlang_nif_sys_api::ErlNifEnv, argc: erlang_nif_sys_api::c_int, _args: *const erlang_nif_sys_api::ERL_NIF_TERM) -> erlang_nif_sys_api::ERL_NIF_TERM {
//         (argc*7) as usize
//     }

//     fn slice_nif(_env: *mut erlang_nif_sys_api::ErlNifEnv, args: &[erlang_nif_sys_api::ERL_NIF_TERM]) -> erlang_nif_sys_api::ERL_NIF_TERM {
//         args.len() * 17
//     }

//     #[test]
//     fn opt_some2() {
//         let entry = get_entry!("empty", [], {load: load, unload:unload})();
//         assert_eq!(0, entry.num_of_funcs);
//         assert_eq!(14, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
//         assert_eq!(None, entry.reload);
//         assert_eq!(None, entry.upgrade);
//         unsafe{entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut())}; // shouldn't panic or crash
//     }

//     #[test]
//     fn nif1() {
//         let entry = get_entry!("nifs", [("raw1", 3, raw_nif1)])();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
//         assert_eq!(1, funcs.len());
//         assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(3,  funcs[0].arity);
//         assert_eq!(28, unsafe{(funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())});
//         assert_eq!(0,  funcs[0].flags);
//     }

//     #[test]
//     fn nif_wrapped() {
//         let entry = get_entry!("nifs", [("sliced", 6, slice_args!(slice_nif))])();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
//         assert_eq!(1, funcs.len());
//         assert_eq!(CString::new("sliced").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(6,  funcs[0].arity);
//         assert_eq!(34, unsafe{(funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())});
//         assert_eq!(0,  funcs[0].flags);
//     }

// }

// #[cfg(test)]
// mod rustermacro_tests {
//     use erlang_nif_sys_api::*;
//     use std;
//     use std::ptr;
//     use std::slice;
//     use std::ffi::{CString, CStr};


//     // Initializer tests

//     fn load(_env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
//         14
//     }

//     fn unload(_env: *mut ErlNifEnv, _priv_data: *mut c_void) {}

//     fn raw_nif1(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
//         (argc*7) as usize
//     }

//     fn raw_nif2(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
//         (argc*11) as usize
//     }

//     fn slice_nif(_env: *mut ErlNifEnv, args: &[ERL_NIF_TERM]) -> ERL_NIF_TERM {
//         args.len() * 17
//     }

//     extern "C" fn c_load(_env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
//         114
//     }

//     extern "C" fn c_unload(_env: *mut ErlNifEnv, _priv_data: *mut c_void) {}

//     extern "C" fn c_nif1(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
//         (argc*19) as usize
//     }

//     unsafe fn unsafe_load(_env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
//         15
//     }

//     unsafe fn unsafe_nif(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
//         (argc*23) as usize
//     }


//     #[test]
//     fn opt_empty() {
//         let entry = get_entry!("empty", [])();
//         assert_eq!(0, entry.num_of_funcs);
//         assert_eq!(None, entry.load);
//         assert_eq!(None, entry.reload);
//         assert_eq!(None, entry.upgrade);
//         assert_eq!(None, entry.unload);
//     }

//     #[test]
//     fn opt_some1() {
//         let entry = get_entry!("empty", [], {load: load})();
//         assert_eq!(0, entry.num_of_funcs);
//         assert_eq!(14, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
//         assert_eq!(None, entry.reload);
//         assert_eq!(None, entry.upgrade);
//         assert_eq!(None, entry.unload);
//     }

//     #[test]
//     fn opt_some2() {
//         let entry = get_entry!("empty", [], {load: load, unload:unload})();
//         assert_eq!(0, entry.num_of_funcs);
//         assert_eq!(14, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
//         assert_eq!(None, entry.reload);
//         assert_eq!(None, entry.upgrade);
//         unsafe{entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut())}; // shouldn't panic or crash
//     }

//     #[test]
//     fn opt_some2b() {  // optionals in different order as opt_some2
//         let entry = get_entry!("empty", [], {unload:unload, load: load})();
//         assert_eq!(0, entry.num_of_funcs);
//         assert_eq!(14, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
//         assert_eq!(None, entry.reload);
//         assert_eq!(None, entry.upgrade);
//         unsafe{entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut())}; // shouldn't panic or crash
//     }

//     #[test]
//     fn opt_closure() {  // optionals in different order as opt_some2
//         let entry = get_entry!("empty", [], {load: |_,_,_|15})();
//         assert_eq!(15, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
//     }


//     #[test]
//     fn modname() {
//         let entry = get_entry!("bananas", [])();
//         assert_eq!(CString::new("bananas").unwrap().as_ref(), unsafe{CStr::from_ptr(entry.name as *const i8)} );
//     }

//     #[test]
//     fn nif1() {
//         let entry = get_entry!("nifs", [("raw1", 3, raw_nif1)])();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
//         assert_eq!(1, funcs.len());
//         assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(3,  funcs[0].arity);
//         assert_eq!(28, unsafe{(funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())});
//         assert_eq!(0,  funcs[0].flags);
//     }

//     #[test]
//     fn nif2() {
//         let entry = get_entry!("nifs", [("raw1", 3, raw_nif1),("raw2", 33, raw_nif2, ERL_NIF_DIRTY_JOB_IO_BOUND)])();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
//         assert_eq!(2, funcs.len());
//         assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(3,  funcs[0].arity);
//         assert_eq!(28, unsafe{(funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())});
//         assert_eq!(0,  funcs[0].flags);
//         assert_eq!(CString::new("raw2").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[1].name as *const i8)});
//         assert_eq!(33,  funcs[1].arity);
//         assert_eq!(44, unsafe{(funcs[1].function)(ptr::null_mut(), 4, ptr::null_mut())});
//         assert_eq!(ERL_NIF_DIRTY_JOB_IO_BOUND,  funcs[1].flags);
//     }

//     #[test]
//     fn nif_closure() {
//         let entry = get_entry!("nifs", [("closure", 5, |_,argc,_| (argc*13) as usize )])();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
//         assert_eq!(1, funcs.len());
//         assert_eq!(CString::new("closure").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(5,  funcs[0].arity);
//         assert_eq!(52, unsafe{(funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())});
//         assert_eq!(0,  funcs[0].flags);
//     }

//     #[test]
//     fn nif_wrapped() {
//         let entry = get_entry!("nifs", [("sliced", 6, slice_args!(slice_nif))])();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
//         assert_eq!(1, funcs.len());
//         assert_eq!(CString::new("sliced").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(6,  funcs[0].arity);
//         assert_eq!(34, unsafe{(funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())});
//         assert_eq!(0,  funcs[0].flags);
//     }

//     #[test]
//     fn legacy() {
//         let entry = get_entry!(
//             b"legacymod\0",
//             Some(c_load), None, None, Some(c_unload),
//             nif!(b"cnif_1\0", 7, c_nif1, ERL_NIF_DIRTY_JOB_IO_BOUND),
//             nif!(b"cnif_2\0", 8, c_nif1))();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};

//         assert_eq!(CString::new("legacymod").unwrap().as_ref(), unsafe{CStr::from_ptr(entry.name as *const i8)} );

//         assert_eq!(114, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
//         assert_eq!(None, entry.reload);
//         assert_eq!(None, entry.upgrade);
//         unsafe{entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut())}; // shouldn't panic or crash

//         assert_eq!(2, funcs.len());

//         assert_eq!(CString::new("cnif_1").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(7,  funcs[0].arity);
//         assert_eq!(38, unsafe{(funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())});
//         assert_eq!(ERL_NIF_DIRTY_JOB_IO_BOUND,  funcs[0].flags);

//         assert_eq!(CString::new("cnif_2").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[1].name as *const i8)});
//         assert_eq!(8,  funcs[1].arity);
//         assert_eq!(57, unsafe{(funcs[1].function)(ptr::null_mut(), 3, ptr::null_mut())});
//         assert_eq!(0,  funcs[1].flags);
//     }

//     #[test]
//     fn trailing_comma() {
//         get_entry!("nifs",
//             [
//                 ("raw1", 3, raw_nif1),
//                 ("raw2", 33, raw_nif2, ERL_NIF_DIRTY_JOB_IO_BOUND),   // <- trailing comma
//             ],
//             {
//                 unload: unload,
//                 load: load,    // <- trailing comma
//             })();

//     }

//     #[test]
//     fn unsafe_callbacks() {
//         let entry = get_entry!("unsafe_nifs",
//             [
//                 ("unsafe_nif", 3, unsafe_nif)
//             ],
//             {
//                 load: unsafe_load
//             })();
//         let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
//         assert_eq!(15, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
//         assert_eq!(CString::new("unsafe_nif").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
//         assert_eq!(3,  funcs[0].arity);
//         assert_eq!(46, unsafe{(funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())});
//         assert_eq!(0,  funcs[0].flags);

//     }


// }
