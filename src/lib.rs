/*!

High level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).


# Overview

Ruster provides high level bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).  It
provides various wrappers to make things behave more Rustic, and it applies Rust's lifetime system to prevent
errors that can be easily made in C NIFs. 

These bindings depend and build upon the crate [Ruster Unsafe](http://goertzenator.github.io/erlang_nif_sys/erlang_nif_sys/index.html).

# For the Impatient

Compile the following in a crate as a dynlib and you will have a functioning NIF module:

```
#[macro_use]
extern crate ruster;

use ruster::*;
use std::result::Result;

// Build NIF function table and nif_init functions.
nif_init!(b"ruster_test\0", None, None, None, None,
  nif!(b"just_123\0", 0, just_123_wrapper)
  );

// Wrapper to present our safe ruster NIF as an unsafe "C" NIF.
unsafe_wrapper!(just_123_wrapper, just_123);

// Our NIF function.  Some key features:
//  - Environment is protected from use outside this NIF by being a reference.
//  - Terms are protected from use outside this NIF by holding a Phantom reference to the Environment.
//  - Returns a Result<>.  An error result will cause enif_make_badarg() to be passed to the caller.
fn just_123<'a>(env:&'a Env, _args:&[Term<'a>]) -> Result<Term<'a>,NifError> {

	//  Convert 123 to term and return as result.
	//  Erlang supported types have Encodable and Decodable traits.
    Ok(123.to_term(env))

}
```

# Env/Term basics

All NIF terms must be associated with an environment, and terms must not be used once their associated
environment has gone out of scope.  To enforce this association, the `Term` type holds a [PhantomData](https://doc.rust-lang.org/std/marker/struct.PhantomData.html)
reference to its environment.  A consequence of this is explicit lifetime markup in various places, for example:

```
fn mynif<'a>(env:&'a Env, args:&[Term<'a>]) -> Result<Term<'a>,NifError> { ...
```
The markup here states that the `Term`s in the `args` slice are associated with `env`, and that the returned
result must also be associated with `env`.  A bit flowery, but it lets the compiler prevent us from breaking
rules that lead to emulator crashes.

The other basic protection mechanism is the scoping of Env in NIF function.  Process dependent environments
are only valid for the duration of a NIF call, and this is enforced in Ruster by providing the environment
as a reference to `Env`.  Any attempt to "save" the environment for use after the NIF call has returned will
be a compile error.


# Encodable and Decodable traits

Conversions between NIF terms and Erlang types are done with implementations
of the Encodable and Decodable traits:

```
pub trait Encodable {
	fn to_term<'a>(&self, env:&'a Env) -> Term<'a>;
}

pub trait Decodable {
	fn from_term(env:& Env, term:Term) -> Result<Self, NifError>;
}
```

`to_term` creates a new term in the given environment with a phantom reference to that environment.
`from_term` converts a term to a Rust type.  `from_term` may fail if the underlying type didn't match
the requested target type, hence the return type is `Result`.  Some examples and error handling strategies:

```
(write me)
```

# Process Independent Environments and Send

The Erlang NIF API also allows you to create additional environments that can live beyond the context of a
NIF call.  These are represented by the type `OwnedEnv`.  An `Env` reference can be acquired with the `get_static`
method.  For example:

```
let oe = OwnedEnv::new();
let env = unsafe{ oe.get_static() };
let term = 123.to_term(env);
...
```

Oh dear.. static? unsafe?  Unfortuneately the safety model for NIF functions breaks down when applied to process 
independent environments (because of Send, see below).  To turn the safties off, the method `get_static()` returns
a `'static` reference to an `Env`.  Any terms created with this static Env reference will also be static, and will
be permitted to outlast the environment.  You must manually ensure that the rules are followed; the compiler will
not catch your mistakes here.  To remind you of this the `get_static()` method is marked `unsafe`.


## Safer PIEs?

Creating safer PIEs is an area of ongoing investigation.  I you have the answer, please tell me!  I investigated
some monad-like constructs, but could not make things fundamentally safer than the system above.

# Static Atoms





# Resources

# Threads

Use of `Env` is not threadsafe, hence it lacks the `Sync` trait.

!*/



//#[macro_use]
extern crate erlang_nif_sys;
extern crate libc;

use erlang_nif_sys::*;
use std::mem::transmute;


// use libc::c_int;
use libc::c_uint;
use libc::c_double;
// use libc::c_long;
// use libc::c_ulong;




/*
Env and Term constraints

1. Terms are associated with an environment and are only valid while the environment is valid.  When manipulating
   terms (encoding or decoding) the environment must always be provided.

2. Exception: Terms that represent an atom are not associated with any environment. (undocumented but frequently used detail)

3. Process bound environments provided to a NIF function becomes invalid after the function returns.

4. Process independent environments can be created with enif_alloc_env().  Terms can be copied to different
   environments with enif_make_copy()
 
5. Terms associated with process independent environment are valid until enif_free_env(), enif_send(), or
   enif_clear_env() are called on the environment.

6. Envs and terms are not thread-safe and must be protected from concurrent access.
*/

/*

Rust design of Terms and Env

Env struct can stay as-is

Term should have a PhantomData reference to an Env
	- This prevents usage of Terms outside and Env.
	- Terms that happen to be atoms are overconstrained.
		- Atoms should be part of a static environment
		- Provide unsafe conversion of bound env term to static env term.
		- Provide runtime checked conversion to static env "fn to_atom_term(env, term) -> Option<Term, Error>".

	- For process independent environments we need to produce a struct with...
		- Drop trait that invokes enif_free_env().
		- *not* Deref.. Envs are opaque and never dereferenced.
		- but... Deref: Dereferences and provides *reference* to *target*.
		   - This inuitively seems wrong.  Grates against C instincts.
		   - But if you think as references in a C++ sense, it is ok.
		   - Deref is a standard op.  Into, AsRef is not.
		- Into?
		- AsRef?
*/


pub use erlang_nif_sys::ErlNifEnv as Env;
pub use erlang_nif_sys::ErlNifPid as Pid;

use std::result;

// pub struct Term {
// 	term: erlang_nif_sys::ERL_NIF_TERM
// }
#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Term<'a> {
	term: erlang_nif_sys::ERL_NIF_TERM,
	phantom: std::marker::PhantomData<&'a Env>  // Created term ends up borrowing env rendering it unavailable for anything else?
}
// pub struct Term<'a> {
// 	term: erlang_nif_sys::ERL_NIF_TERM,
// 	phantom: std::marker::PhantomData<&'a ()>
// }



// // won't work because neither Drop and Unique are from this module.
// pub impl Drop for Unique<Env> {
// 	 fn drop(&mut self) {
//         println!("Dropping env ptr");
//         unsafe{ erlang_nif_sys::enif_free_env(self) };
//     }	
// }
// pub impl Unique<Env> {
// 	fn new() -> Unique<Env> {
// 		Unique::<Env>::new(unsafe{erlang_nif_sys::enif_alloc_env()})
//  	}
// }


#[derive(Debug)]
pub struct UniqueEnv {
	env: *mut Env
}

impl UniqueEnv {
	pub fn new() -> UniqueEnv {
		UniqueEnv{ env: unsafe{erlang_nif_sys::enif_alloc_env()}}
	}
	pub fn clear(&self) {
		unsafe { enif_clear_env(self.env); }
	}

	pub fn as_static(&self) -> &'static Env {
		unsafe{ &*(self.env) }
	}
}

impl Drop for UniqueEnv {
	 fn drop(&mut self) {
        println!("Dropping env ptr");
        unsafe{ erlang_nif_sys::enif_free_env(self.env) };
    }
}

impl AsRef<Env> for UniqueEnv {
	fn as_ref(&self) -> &Env {
		unsafe{ &*(self.env) }
	}
}

// impl std::ops::Deref for UniqueEnv {
// 	type Target = Env;
// 	fn deref<'a>(&'a self) -> &'a Self::Target {
// 		unsafe{transmute(self.env)}
// 	}
// }

// impl std::ops::DerefMut for EnvPtr {
// 	fn deref_mut<'a>(&'a mut self) -> &'a mut Self::Target {
// 		unsafe{transmute(self.env)}
// 	}
// }

// pub struct Pie1 {
// 	envptr: EnvPtr,
// 	erlang_nif_sys::ERL_NIF_TERM
// }

// pub struct OwnedEnv {
// 	ptr: EnvPtr
// }
// impl OwnedEnv {
// 	pub fn new() -> OwnedEnv {
// 		OwnedEnv{ ptr: EnvPtr::new() }
// 	}
// 	pub unsafe fn get_static(&self) -> &'static Env {
// 		&*self.ptr.env
// 	}
// }

#[derive(Debug)]
pub struct InvalidEnv {
	env: UniqueEnv
}
impl InvalidEnv {
	pub fn clear(self) -> UniqueEnv {
		self.env
	}
}



// pub enum SendResult {
// 	Ok(InvalidEnv),
// 	Err(OwnedEnv)
// }

// impl SendResult {
// 	pub fn unwrap(self) -> InvalidEnv {
// 		match self {
// 			SendResult::Ok(env) => env,
// 			SendResult::Err(_) => panic!("SendResult unwrap failed")
// 		}
// 	}
// }


pub unsafe fn send_from_thread(to_pid:&Pid, msg_env:UniqueEnv, msg:Term<'static>) -> result::Result<InvalidEnv, UniqueEnv> {
	
	match enif_send(std::ptr::null_mut(), &*to_pid, msg_env.env, msg.term) {
		0 => Err(msg_env),
		_ => Ok(InvalidEnv{env: msg_env})
	}
}

// impl AsMut<Env> for EnvPtr {
//     fn as_mut(&mut self) -> &mut Env {
//     	unsafe{transmute(self.env)}
//     }
// }








/// Get pid of current process.
/// Underlying nif implementation will provide an invalid PID if the
/// provided environment is process independent.
pub fn selfpid(env:&Env) -> Pid {
	unsafe {
		let mut pid:Pid = std::mem::uninitialized();
		enif_self(transmute(env), &mut pid);
		pid		
	}
}


// /// Send message and free sending environment.
// pub fn send_from_process(env:&Env, pid:&Pid, msg_env:EnvPtr, msg:Term) {
// 	send_from_process_main(&env, &pid, msg_env, msg);	
// }


// /// Send message and recycle environment (enif_clear_env())
// pub fn send_from_process_recycle(env:&Env, pid:&Pid, msg_env:EnvPtr, msg:Term) -> EnvPtr {
// 	let msg_env2 = send_from_process_main(&pid, msg_env, msg);	
// 	unsafe {enif_clear_env(msg_env2);}
// 	msg_env
// }

// fn send_from_process_main(env:&Env, pid:&Pid, msg_env:EnvPtr, msg:Term) -> EnvPtr {
// 	enif_send(env, &pid, &msg_env, msg);
// 	msg_env
// }






#[derive(Debug, Copy, Clone)]
pub enum Error {
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


pub type Result<T> = result::Result<T, Error>;



pub trait Encodable {
	fn to_term<'a>(&self, env:&'a Env) -> Term<'a>;
}

pub trait Decodable: Sized {
	fn from_term(env:& mut Env, term:Term) -> Result<Self>;
}




#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Term<'a> {
	term: erlang_nif_sys::ERL_NIF_TERM,
	phantom: std::marker::PhantomData<&'a Env>  // Created term ends up borrowing env rendering it unavailable for anything else?
}


#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Atom {
	term: erlang_nif_sys::ERL_NIF_TERM,
}

//	assert_eq!(std::mem::size_of::<Term>(), std::mem::size_of::<ERL_NIF_TERM>());


/// Return term as an environment-less atom term.
///
/// The caller must assure that the input term in fact represents and atom.
fn unsafe_term_to_atom(term:Term) -> Term<'static> {
	unsafe{transmute(term)}
}

/// Return term as an environment-less atom term.
///
/// The caller must assure that the input term in fact represents and atom.
pub fn term_to_atom(env:&mut Env, term:Term) -> Option<Term<'static>> {
	match unsafe{enif_is_atom(transmute(env), transmute(term))} {
		0 => None,
		_ => Some(unsafe_term_to_atom(term)),
	}
}



// impl Encodable for c_int {
// 	fn to_term<'a>(&self, env:&'a mut Env) -> Term<'a> {
// 		unsafe{ transmute(erlang_nif_sys::enif_make_int(transmute(env), *self)) }
// 	}
// }
// impl Decodable for c_int {
// 	fn from_term(env:&Env, term:Term) -> Result<Self, NifError> {
// 		unsafe {
// 			let mut result: Self = 0;
// 			match erlang_nif_sys::enif_get_int(transmute(env), transmute(term), &mut result) {
// 				0 => Err(NifError::Badarg),
// 				_ => Ok(result),
// 			}
// 		}
// 	}
// }


macro_rules! implement_simple_encodable {
	($enctype:ty, $fun:path) => (
		impl Encodable for $enctype {
			fn to_term<'a>(&self, env:&'a Env) -> Term<'a>{
				unsafe{ transmute($fun(transmute(env), *self)) }
			}
		}
	)
}

macro_rules! implement_simple_decodable{
	($dectype:ty, $fun:path) => (

		impl Decodable for $dectype {
			fn from_term(env:& mut Env, term:Term) -> Result<Self> {
				unsafe {
					let mut result: Self = std::mem::uninitialized();
					match $fun(transmute(env), transmute(term), &mut result) {
						0 => Err(Error::Badarg),
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

// implement_simple_encodable!(c_int, erlang_nif_sys::enif_make_int);
// implement_simple_decodable!(c_int, erlang_nif_sys::enif_get_int);

implement_simple_transcodable!(c_int, enif_make_int, enif_get_int);
implement_simple_transcodable!(c_uint, enif_make_uint, enif_get_uint);
implement_simple_transcodable!(c_double, enif_make_double, enif_get_double);
implement_simple_transcodable!(i64, enif_make_int64, enif_get_int64);
implement_simple_transcodable!(u64, enif_make_uint64, enif_get_uint64);
// implement_simple_transcodable!(c_long, erlang_nif_sys::enif_make_long, erlang_nif_sys::enif_get_long);
// implement_simple_transcodable!(c_ulong, erlang_nif_sys::enif_make_ulong, erlang_nif_sys::enif_get_ulong);


// Tuple implementation


// Slice of Term

impl<'b> Encodable for &'b[Term<'b>] {
	fn to_term<'a>(&self, env:&'a Env) -> Term<'a> {
		unsafe {
			transmute(
				enif_make_tuple_from_array(
					transmute(env),
					self.as_ptr() as *const ERL_NIF_TERM,
					self.len() as c_uint)
			)
		}
	}
}

impl<'b> Decodable for &'b[Term<'b>] {
	fn from_term(env:& mut Env, term:Term) -> Result<Self> {
		unsafe {
			let mut arity:c_int = std::mem::uninitialized();
			let mut array:*const erlang_nif_sys::ERL_NIF_TERM = std::mem::uninitialized();
			match enif_get_tuple(transmute(env), transmute(term), &mut arity, &mut array) {
				0 => Err(Error::Badarg),
				_ => Ok(std::slice::from_raw_parts(array as *const Term, arity as usize)),
			}
		}
	}
}


// Arbitrary Tuples

impl<T0: Encodable, T1: Encodable> Encodable for (T0,T1) {
	fn to_term<'a>(&self, env:&'a Env) -> Term<'a> {
		let terms = [self.0.to_term(env), self.1.to_term(env)];
		terms.as_ref().to_term(env)
	}
}

// impl<T0: Decodable, T1: Decodable> Decodable for (T0,T1) {

// 	fn from_term(env:& mut Env, term:Term) -> Result<Self> {
// 		let terms:&[Term] = try!(
// 			from_term(env, term).
// 			and_then(|sl| match sl.len() {2 => Ok(sl), _ => Err(Error::Badarg) }));

// 		(
// 			try!(from_term(env, terms[0])),
// 			try!(from_term(env, terms[1])),
// 		)
// 	}
// }



// List Iterator

// struct<'a> ListDecoder {
// 	env: &'a Env,
// 	tail: ERL_NIF_TERM,
// }

// impl ListDecoder {
// 	fn new<'a>(env:&'a mut Env, term: Term<'a>) -> Result<ListDecoder<'a>> {
// 		unsafe {
// 			match enif_is_list(transmute(env), transmute(term)) {
// 				0 => Err(Error::Badarg),
// 				_ => Ok(ListDecoder{env: env, tail: transmute(term)})
// 			}
// 		}
// 	}
// }

// impl Iterator<'a> for ListDecoder<'a> {
// 	type Item = Term<'a>;
// 	fn next(&mut self) -> Option<Self::Item> {
// 		unsafe {
// 			let mut head = uninitialized();
// 			let mut newtail = uninitialized();
// 			match enif_get_list_cell(self.env, self.tail, &mut head, &mut newtail) {
// 				0 => None,
// 				_ => {
// 					self.tail = newtail;
// 					Some(transmute(head))
// 				},
// 			}
// 		}
// 	}
// }





//non-simple transcoders to implement:
// atom
// local_pid
// list, list cell, list length
// resource
// string
// binary, sub binary
// make ref


pub type RusterFnType<'a> = fn(env:&'a Env, args:&[Term<'a>]) -> Result<Term<'a>>;
//type blah_type = fn(blah:i32) -> i32;


// #[inline]
// pub fn ruster_fn_wrapper(env: *mut erlang_nif_sys::ErlNifEnv,
// 		                    argc: erlang_nif_sys::c_int,
// 		                    args: *const erlang_nif_sys::ERL_NIF_TERM,
// 		                    ruster_fn: RusterFnType,
// 		                    ) -> erlang_nif_sys::ERL_NIF_TERM {
//     unsafe {
//     	//println!("input arg = {:?}", *args);
//         match ruster_fn(transmute(env), std::slice::from_raw_parts(args as *const Term, argc as usize)) {
//             Ok(x) => {
//            		let result = std::mem::transmute(x);
//     			//println!("return = {:?}", x);
//     			result
//     		},
//             _ => erlang_nif_sys::enif_make_badarg(env),
//         }
//     }
// }


// #[macro_export]
// macro_rules! unsafe_wrapper {
// 	($wrapper:ident, $wrappee:ident) => (
// 		extern "C" fn $wrapper(env: *mut erlang_nif_sys::ErlNifEnv,
// 		                          argc: erlang_nif_sys::c_int,
// 		                          args: *const erlang_nif_sys::ERL_NIF_TERM) -> erlang_nif_sys::ERL_NIF_TERM
// 		{
// 			$crate::ruster_fn_wrapper(env, argc, args, $wrappee)
// 		}
// 	)
// }




#[macro_export]
macro_rules! nif_wrapper {
	($wrapper:ident, $wrappee:ident) => (
		extern "C" fn $wrapper(env: *mut erlang_nif_sys::ErlNifEnv,
		                          argc: erlang_nif_sys::c_int,
		                          args: *const erlang_nif_sys::ERL_NIF_TERM) -> erlang_nif_sys::ERL_NIF_TERM {
		    unsafe {
		        match $wrappee(std::mem::transmute(env), std::slice::from_raw_parts(args as *const Term, argc as usize)) {
		            Ok(x) => std::mem::transmute(x),
		            _     => erlang_nif_sys::enif_make_badarg(env),
		        }
		    }		
		}
	)
}


