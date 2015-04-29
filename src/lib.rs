
//#[macro_use]
extern crate ruster_unsafe;
extern crate libc;

use ruster_unsafe::*;
use std::mem::transmute;
//use std::error;

use std::ops::Deref;

// use libc::c_int;
use libc::c_uint;
use libc::c_double;
// use libc::c_long;
// use libc::c_ulong;




/*
Environment and Term constraints

1. Terms are associated with an environment and are only valid while the environment is valid.  When manipulating
   terms (encoding or decoding) the environment must always be provided.

2. Exception: Terms that represent an atom are not associated with any environment. (undocumented but frequently used detail)

3. Process bound environments provided to a NIF function becomes invalid after the function returns.

4. Process independent environments can be created with enif_alloc_env().  Terms can be copied to different
   environments with enif_make_copy()
 
5. Terms associated with process independent environment are valid until enif_free_env(), enif_send(), or
   enif_clear_env() are called on the environment.

6. Environments and terms are not thread-safe and must be protected from concurrent access.
*/

/*

Rust design of Terms and Environment

Environment struct can stay as-is

Term should have a PhantomData reference to an Environment
	- This prevents usage of Terms outside and Environment.
	- Terms that happen to be atoms are overconstrained.
		- Atoms should be part of a static environment
		- Provide unsafe conversion of bound env term to static env term.
		- Provide runtime checked conversion to static env "fn to_atom_term(env, term) -> Option<Term, Error>".

	- For process independent environments we need to produce a struct with...
		- Drop trait that invokes enif_free_env().
		- *not* Deref.. Environments are opaque and never dereferenced.
		- but... Deref: Dereferences and provides *reference* to *target*.
		   - This inuitively seems wrong.  Grates against C instincts.
		   - But if you think as references in a C++ sense, it is ok.
		   - Deref is a standard op.  Into, AsRef is not.
		- Into?
		- AsRef?
*/


pub use ruster_unsafe::ErlNifEnv as Environment;
pub use ruster_unsafe::ErlNifPid as Pid;


pub struct EnvironmentPtr {
	env: *const Environment
}

unsafe impl Send for EnvironmentPtr {}

impl EnvironmentPtr {
	pub fn new() -> EnvironmentPtr {
		EnvironmentPtr{ env: unsafe{ruster_unsafe::enif_alloc_env()}}
	}
}

impl Drop for EnvironmentPtr {
	 fn drop(&mut self) {
        println!("Dropping env");
        unsafe{ ruster_unsafe::enif_free_env(self.env) };
    }
}

// impl AsMut<Environment> for EnvironmentPtr {
//     fn as_mut(&mut self) -> &mut Environment {
//     	unsafe{transmute(self.env)}
//     }
// }


impl Deref for EnvironmentPtr {
	type Target = Environment;
	fn deref<'a>(&'a self) -> &'a Self::Target {
		unsafe{transmute(self.env)}
	}
}

// impl DerefMut for EnvironmentPtr {
// 	fn deref_mut<'a>(&'a mut self) -> &'a mut Self::Target {
// 		unsafe{transmute(self.env)}
// 	}
// }

// pub struct Term {
// 	term: ruster_unsafe::ERL_NIF_TERM
// }
#[derive(Copy, Clone, Debug)]
#[repr(C)]
// pub struct Term<'a> {
// 	term: ruster_unsafe::ERL_NIF_TERM,
// 	phantom: std::marker::PhantomData<&'a Environment>  // Created term ends up borrowing env rendering it unavailable for anything else?
// }
pub struct Term<'a> {
	term: ruster_unsafe::ERL_NIF_TERM,
	phantom: std::marker::PhantomData<&'a ()>
}


//	assert_eq!(std::mem::size_of::<Term>(), std::mem::size_of::<ERL_NIF_TERM>());


// /// Return term as an environment-less atom term.
// ///
// /// The caller must assure that the input term in fact represents and atom.
// pub fn unsafe_term_to_atom(term:Term) -> Term<'static> {
// 	unsafe{transmute(term)}
// }

// /// Return term as an environment-less atom term.
// ///
// /// The caller must assure that the input term in fact represents and atom.
// pub fn term_to_atom(env:&mut Environment, term:Term) -> Option<Term<'static>> {
// 	match unsafe{enif_is_atom(transmute(env), transmute(term))} {
// 		0 => None,
// 		_ => Some(unsafe_term_to_atom(term)),
// 	}
// }


/// Get pid of current process.
pub fn selfpid(env:&Environment) -> Pid {
	unsafe {
		let mut pid:Pid = std::mem::uninitialized();
		enif_self(env, &mut pid);
		pid		
	}
}

/// Send message and free sending environment.
pub fn send_from_thread(pid:&Pid, msg_env:EnvironmentPtr, msg:Term) {
	send_from_thread_main(&pid, msg_env, msg);	
}

/// Send message and recycle environment (enif_clear_env())
pub fn send_from_thread_recycle(pid:&Pid, msg_env:EnvironmentPtr, msg:Term) -> EnvironmentPtr {
	let msg_env2 = send_from_thread_main(&pid, msg_env, msg);	
	unsafe {enif_clear_env(&*msg_env2);}
	msg_env2
}

fn send_from_thread_main(pid:&Pid, msg_env:EnvironmentPtr, msg:Term) -> EnvironmentPtr {
	unsafe {
		enif_send(std::ptr::null(), pid, &*msg_env, transmute(msg));
	}
	msg_env
}


pub fn kill_ep(_msg_env:EnvironmentPtr) {}


// /// Send message and free sending environment.
// pub fn send_from_process(env:&Environment, pid:&Pid, msg_env:EnvironmentPtr, msg:Term) {
// 	send_from_process_main(&env, &pid, msg_env, msg);	
// }


// /// Send message and recycle environment (enif_clear_env())
// pub fn send_from_process_recycle(env:&Environment, pid:&Pid, msg_env:EnvironmentPtr, msg:Term) -> EnvironmentPtr {
// 	let msg_env2 = send_from_process_main(&pid, msg_env, msg);	
// 	unsafe {enif_clear_env(msg_env2);}
// 	msg_env
// }

// fn send_from_process_main(env:&Environment, pid:&Pid, msg_env:EnvironmentPtr, msg:Term) -> EnvironmentPtr {
// 	enif_send(env, &pid, &msg_env, msg);
// 	msg_env
// }






#[derive(Debug, Copy, Clone)]
pub enum NifError {
	Badarg,
}

// impl error::Error for NifError {
// 	fn description(&self) -> &str {
// 		match *self {
// 			NifError::Badarg => "bad argument",
// 		}
// 	}
// }


// stuct Badarg;

// impl error::Error for Badarg {
// 	fn description(&self) -> &str {
// 		"bad argument"
// 	}
// }

pub trait Encodable {
	fn to_term<'a>(&self, env:&'a Environment) -> Term<'a>;
}

pub trait Decodable {
	fn from_term(env:& mut Environment, term:Term) -> Result<Self, NifError>;
}


// impl Encodable for c_int {
// 	fn to_term<'a>(&self, env:&'a mut Environment) -> Term<'a> {
// 		unsafe{ transmute(ruster_unsafe::enif_make_int(transmute(env), *self)) }
// 	}
// }
// impl Decodable for c_int {
// 	fn from_term(env:&Environment, term:Term) -> Result<Self, NifError> {
// 		unsafe {
// 			let mut result: Self = 0;
// 			match ruster_unsafe::enif_get_int(transmute(env), transmute(term), &mut result) {
// 				0 => Err(NifError::Badarg),
// 				_ => Ok(result),
// 			}
// 		}
// 	}
// }


macro_rules! implement_simple_encodable {
	($enctype:ty, $fun:path) => (
		impl Encodable for $enctype {
			fn to_term<'a>(&self, env:&'a Environment) -> Term<'a>{
				unsafe{ transmute($fun(transmute(env), *self)) }
			}
		}
	)
}

macro_rules! implement_simple_decodable{
	($dectype:ty, $fun:path) => (

		impl Decodable for $dectype {
			fn from_term(env:& mut Environment, term:Term) -> Result<Self, NifError> {
				unsafe {
					let mut result: Self = std::mem::uninitialized();
					match $fun(transmute(env), transmute(term), &mut result) {
						0 => Err(NifError::Badarg),
						_ => Ok(result),
					}
				}
			}
		}
	)
}

macro_rules! implement_simple_transcodable {
	($transtype:ty, $encfun:path, $decfun:path) => (
		implement_simple_encodable!($transtype, $encfun);
		implement_simple_decodable!($transtype, $decfun);
	)
}

// implement_simple_encodable!(c_int, ruster_unsafe::enif_make_int);
// implement_simple_decodable!(c_int, ruster_unsafe::enif_get_int);

implement_simple_transcodable!(c_int, enif_make_int, enif_get_int);
implement_simple_transcodable!(c_uint, enif_make_uint, enif_get_uint);
implement_simple_transcodable!(c_double, enif_make_double, enif_get_double);
implement_simple_transcodable!(i64, enif_make_int64, enif_get_int64);
implement_simple_transcodable!(u64, enif_make_uint64, enif_get_uint64);
// implement_simple_transcodable!(c_long, ruster_unsafe::enif_make_long, ruster_unsafe::enif_get_long);
// implement_simple_transcodable!(c_ulong, ruster_unsafe::enif_make_ulong, ruster_unsafe::enif_get_ulong);


// Tuple implementation

// macro_rules! tuple {
// 	() => ();
// 	( $($name:ident,)+) => (
// //		impl<$($name:Decodable),*> Decodable for ($($name,)*) {
// 		impl Decodable for ($($name,)*) {
// //	        #[allow(non_snake_case)]
// 			fn from_term(env:&Environment, term:Term) -> Result<Self, NifError> {
// 				unsafe {
// 					let mut arity:c_int = std::mem::uninitialized();
// 					let mut array:*const ruster_unsafe::ERL_NIF_TERM = std::mem::uninitialized();
// 					match enif_get_tuple(transmute(env), transmute(term), &arity, &array)
// 				}
// 				let ret = ($(try!(blah $name)))


// 	        fn decode<D: Decoder>(d: &mut D) -> Result<($($name,)*), D::Error> {
// 	            let len: uint = count_idents!($($name,)*);
// 	            d.read_tuple(len, |d| {
// 	                let mut i = 0;
// 	                let ret = ($(try!(d.read_tuple_arg({ i+=1; i-1 },
// 	                                                   |d| -> Result<$name,D::Error> {
// 	                    Decodable::decode(d)
// 	                })),)*);
// 	                return Ok(ret);
// 	            })
// 	        }
// 	    }
// 	    impl<$($name:Encodable),*> Encodable for ($($name,)*) {
// 	        #[allow(non_snake_case)]
// 	        fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
// 	            let ($(ref $name,)*) = *self;
// 	            let mut n = 0;
// 	            $(let $name = $name; n += 1;)*
// 	            s.emit_tuple(n, |s| {
// 	                let mut i = 0;
// 	                $(try!(s.emit_tuple_arg({ i+=1; i-1 }, |s| $name.encode(s)));)*
// 	                Ok(())
// 	            })
// 	        }
// 	    }
// 	    peel! { $($name,)* }
// 		)
// }

// let ret = ($(try!(d.read_tuple_arg({ i+=1; i-1 },
//                                                        |d| -> Result<$name,D::Error> {
//                         Decodable::decode(d)
//                     })),)*);
//                     return Ok(ret);

// impl<T0: Decodable, T1: Decodable> Decodable for (T0,T1) {
// 	fn from_term(env:& mut Environment, term:Term) -> Result<Self, NifError> {
// 		unsafe {
// 			let arity:c_int = std::mem::uninitialized();
// 			let array:*const ERL_NIF_TERM = std::mem::uninitialized();
// 			match enif_get_tuple(env,
// 				 std::mem::transmute(term), std::mem::transmute(&arity), std::mem::transmute(&array)) {
// 				0 => Err(NifError::Badarg),
// 				_ => {
// 						if arity != 2 {
// 							Err(NifError::Badarg)
// 						}
// 						else {
// 							Ok(( try!(Decodable::from_term(env, std::mem::transmute(*(array.offset(0))))),
// 							     try!(Decodable::from_term(env, std::mem::transmute(*(array.offset(1)))))
// 							  ))	
// 						}
// 					}
// 				}
// 			}
// 		}
// 	}


  // fn from_term(term: u32) -> Option<(T0,T1)> {
  //  let ts = [5,3];
  //  match TermTranscodeable::from_term(ts[0]) {
  //    Some(u0) =>
  //     match TermTranscodeable::from_term(ts[1]) {
  //     Some(u1) => Some((u0,u1)),
  //     _ => None,
  //    },
  //    _ => None,
  //  }
  // }
//   fn from_term(term: u32) -> Option<(T0,T1)> {
//      let ts = [5,3];
//      Some((
//       match TermTranscodeable::from_term(ts[0]) {
//         Some(u0) => u0,
//         _ => return None,
//       },
//       match TermTranscodeable::from_term(ts[1]) {
//         Some(u1) => u1,
//         _ => return None,
//       }
//       ))
//   }

//   fn to_term(&self) -> u32 {
//      6
//   }
// }




//non-simple transcoders to implement:
// atom
// local_pid
// list, list cell, list length
// resource
// string
// binary, sub binary
// make ref


// #[macro_export]
// macro_rules! wrap_l2_nif {
// 	($wrappee:ident) => (
// 		#[no_mangle]
// 		pub extern "C" fn concat_idents!($wrapee, _wrapped)(env: *mut ErlNifEnv,
// 		                          argc: c_int,
// 		                          args: *const ERL_NIF_TERM) -> ERL_NIF_TERM
// 		{
// 		    unsafe {
// 		         match $wrapee(&*env, std::slice::from_raw_buf(transmute(&args), argc as usize)) {
// 		            Ok(x) => transmute(x),
// 		            _ => enif_make_badarg(env),
// 		         }
// 		    }
// 		}
// 	)
// }


pub type RusterFnType<'a> = fn(env:&'a Environment, args:&[Term<'a>]) -> Result<Term<'a>,NifError>;
//type blah_type = fn(blah:i32) -> i32;


#[inline]
pub fn ruster_fn_wrapper(env: *mut ruster_unsafe::ErlNifEnv,
		                    argc: ruster_unsafe::c_int,
		                    args: *const ruster_unsafe::ERL_NIF_TERM,
		                    ruster_fn: RusterFnType,
		                    ) -> ruster_unsafe::ERL_NIF_TERM {
    unsafe {
    	//println!("input arg = {:?}", *args);
        match ruster_fn(transmute(env), std::slice::from_raw_parts(args as *const Term, argc as usize)) {
            Ok(x) => {
           		let result = std::mem::transmute(x);
    			//println!("return = {:?}", x);
    			result
    		},
            _ => ruster_unsafe::enif_make_badarg(env),
        }
    }
}


#[macro_export]
macro_rules! unsafe_wrapper {
	($wrapper:ident, $wrappee:ident) => (
		extern "C" fn $wrapper(env: *mut ruster_unsafe::ErlNifEnv,
		                          argc: ruster_unsafe::c_int,
		                          args: *const ruster_unsafe::ERL_NIF_TERM) -> ruster_unsafe::ERL_NIF_TERM
		{
			$crate::ruster_fn_wrapper(env, argc, args, $wrappee)
		}
	)
}

// #[macro_export]
// macro_rules! unsafe_wrapper {
// 	($wrapper:ident, $wrappee:ident) => (
// 		extern "C" fn $wrapper(env: *mut ruster_unsafe::ErlNifEnv,
// 		                          argc: ruster_unsafe::c_int,
// 		                          args: *const ruster_unsafe::ERL_NIF_TERM) -> ruster_unsafe::ERL_NIF_TERM
// 		{
// 		    unsafe {
// 		        match $wrappee(&*env, std::slice::from_raw_parts(std::mem::transmute(&args), argc as usize)) {
// 		           Ok(x) => std::mem::transmute(x),
// 		           _ => ruster_unsafe::enif_make_badarg(env),
// 		        }
// 		    }
// 		}
// 	)
// }

// /// Create ErlNifFunc structure.  Use inside `nif_init!`.
// #[macro_export]
// macro_rules! nif{
//     ($name:expr, $arity:expr, $function:expr, $flags:expr) => (
//         ruster_unsafe::ErlNifFunc { name:     $name as *const u8,
//                      arity:    $arity,
//                      function: $function,
//                      flags:    $flags});

//     ($name:expr, $arity:expr, $function:expr) => (
//         nif!($name, $arity, $function, 0))
// }

// #[doc(hidden)]
// #[macro_export]
// macro_rules! count_expr {
//     () => { 0 };
//     ($_e:expr) => { 1 };
//     ($_e:expr, $($rest:expr),+) => { 1 + count_expr!($($rest),*) }
// }


// /// Register NIFs and supporting functions for your module.
// #[macro_export]
// macro_rules! nif_init {
//     ($module:expr, $load:expr, $reload:expr, $upgrade:expr, $unload:expr, $($func:expr),* ) => (
//         const NUM_FUNCS: usize = count_expr!($($func),*);
//         const FUNCS: [$crate::ErlNifFunc; NUM_FUNCS] = [$($func),*];
//         static mut ENTRY: ruster_unsafe::ErlNifEntry = ruster_unsafe::ErlNifEntry{
//             major : NIF_MAJOR_VERSION,
//             minor : NIF_MINOR_VERSION,
//             name : $module as *const u8,
//             num_of_funcs : NUM_FUNCS as c_int,
//             funcs : &FUNCS as *const ruster_unsafe::ErlNifFunc,
//             load :    $load,
//             reload :  $reload,
//             upgrade : $upgrade,
//             unload :  $unload,
//             vm_variant : b"beam.vanilla\0" as *const u8,
//             options: 0,
//         };

//         #[no_mangle]
//         pub extern "C" fn nif_init() -> *const ruster_unsafe::ErlNifEntry {
//             unsafe {&ENTRY}
//         }
//     )
// }


// #[macro_export]
// macro_rules! nif {
// 	($($args:expr),*) => { $crate::nif!($($args),*) };
// }

// #[macro_export]
// macro_rules! nif_init {
// 	($($args:expr),*) => { $crate::nif_init!($($args),*) };
// }

// #[doc(hidden)]
// #[macro_export]
// macro_rules! count_expr {
//     () => { 0 };
//     ($_e:expr) => { 1 };
//     ($_e:expr, $($rest:expr),+) => { 1 + count_expr!($($rest),*) }
// }


// /// Create ErlNifFunc structure.  Use inside `nif_init!`.
// #[macro_export]
// macro_rules! ruster_nif{
//     ($name:expr, $arity:expr, $function:expr, $flags:expr) => (
//         ErlNifFunc { name:     $name as *const u8,
//                      arity:    $arity,
//                      function: concat_idents!($function, _unsafe_wrapper),
//                      flags:    $flags});

//     ($name:expr, $arity:expr, $function:expr) => (
//         ruster_nif!($name, $arity, $function, 0))
// }

// /// Register NIFs and supporting functions for your module.
// #[macro_export]
// macro_rules! ruster_init {
//     ($module:expr, $load:expr, $reload:expr, $upgrade:expr, $unload:expr, $($func:expr),* ) => (
//         const NUM_FUNCS: usize = count_expr!($($func),*);
//         const FUNCS: [ErlNifFunc; NUM_FUNCS] = [$($func),*];
//         static mut ENTRY: ErlNifEntry = ErlNifEntry{
//             major : NIF_MAJOR_VERSION,
//             minor : NIF_MINOR_VERSION,
//             name : $module as *const u8,
//             num_of_funcs : NUM_FUNCS as c_int,
//             funcs : &FUNCS as *const ErlNifFunc,
//             load :    $load,
//             reload :  $reload,
//             upgrade : $upgrade,
//             unload :  $unload,
//             vm_variant : b"beam.vanilla\0" as *const u8,
//             options: 0,
//         };

//         #[no_mangle]
//         pub extern "C" fn nif_init() -> *const ErlNifEntry {
//             unsafe {&ENTRY}
//         }
//     )
// }
