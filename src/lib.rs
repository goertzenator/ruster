/*!

High level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).


# Overview

Ruster provides high level bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).

These bindings depend and build upon the crate [erlang_nif-sys](http://goertzenator.github.io/erlang_nif-sys/erlang_nif-sys/index.html).

# For the Impatient

Compile the following in a crate as a dylib or cdylib and you will have a functioning NIF module:

[totally obsolete]
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
fn just_123(env: &mut Env, _args: TermSlice) -> Result<Term, NifError> {

    //  Convert 123 to term and return as result.
    //  Erlang supported types have Encodable and Decodable traits.
    Ok(123.to_term(env))

}
```


# Conversions to and from Terms

The standard Rust conversion traits From/Into and TryFrom/TryInto are used for conversions in both directions.
All Erlang conversion must be done in the presence of an Env (environment) parameter.  Ruster handles this by
forming a `Binder` struct containing the Env and the source type and using that as the source for the conversion.
All convertible types have a `bind()` method courtesy of the `Bind` trait.


# Term, UncheckedTerm, and CheckedTerm
When a NIF module is compiled in debug mode, the `Term` type is an alias for `CheckedTerm`.  `CheckedTerm` does
runtime checking to assure that it is always used with the environment that is was created with.  A violation
will create a `panic!` that along with a backtrace makes it easy to find code problems.  Environment violations
are otherwise very difficult to track down as they typically manifest as emulator crashes at some random point
in time after the actual violation.

When compiling in release mode, `Term` is an alias for `UncheckedTerm` which removes the safety net (and
the environment checking overhead).



# Encodable and Decodable traits

Conversions between NIF terms and Erlang types are done with implementations
of the Encodable and Decodable traits:

```
pub trait Encodable {
    fn to_term(&self, env: &mut Env) -> Term;
}

pub trait Decodable {
    fn from_term(env: &mut Env, term:Term) -> Result<Self, NifError>;
}
```

`to_term` creates a new term in the given environment.
`from_term` converts a term to a Rust type.  `from_term` may fail if the underlying type didn't match
the requested target type, hence the return type is `Result`.  Some examples and error handling strategies:

```
(write me)
```

# Process Independent Environments and Send

The Erlang NIF API also allows you to create additional environments that can live beyond the context of a
NIF call.  These are represented by the type `OwnedEnv`.


# Static Atoms





# Resources

# Threads

Use of `Env` is not threadsafe, hence it lacks the `Sync` trait.

!*/


//#[macro_use]
pub extern crate erlang_nif_sys;

extern crate unreachable;

use erlang_nif_sys as ens;

// required for macros to work
pub use erlang_nif_sys as erlang_nif_sys_api;


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


#[macro_use]
mod initmacro;

#[macro_use]
mod rustermacro;

//pub use erlang_nif_sys::ErlNifEnv as Env;

#[repr(C)]
pub struct Env(erlang_nif_sys::ErlNifEnv);

impl Env {
    // FIXME no pub
    pub fn as_api_ptr(&self) -> *mut erlang_nif_sys::ErlNifEnv {
        &(self.0) as *const erlang_nif_sys::ErlNifEnv as *mut erlang_nif_sys::ErlNifEnv
    }

    // FIXME, make not pub
    pub fn from_api_ptr<'a>(penv: *mut erlang_nif_sys::ErlNifEnv) -> &'a Self {
        unsafe{ &*(penv as *mut Env) }
    }
}

pub use erlang_nif_sys::ErlNifPid as Pid;


/// Convenience name for low level term type.
pub use erlang_nif_sys::ERL_NIF_TERM as CTerm;


// #[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
// pub struct Term<'a> {
//     term: CTerm,
//     _env: std::marker::PhantomData<&'a Env>,
// }

#[derive(Copy, Clone)]
pub struct ScopedTerm<'a>(CTerm, std::marker::PhantomData<&'a Env>);

#[derive(Copy, Clone)]
pub struct StaticTerm(CTerm);


pub const UNINITIALIZED_STATIC_TERM:StaticTerm = StaticTerm(0);

// struct OwnedEnv {
//  env: *mut Env,
// }

// pub struct MobileTerm {
//  env: OwnedEnv,
//  term: CTerm,
// }


use std::result;



impl<'a> ScopedTerm<'a> {
    // FIXME: remove pub
    pub fn new(ct: CTerm) -> Self {
        ScopedTerm(ct, std::marker::PhantomData)
    }
}

// impl From<ScopedTerm> for CTerm // orphan rule

impl<'a> Into<CTerm> for ScopedTerm<'a> {
    fn into(self) -> CTerm {
        self.0
    }
}


impl StaticTerm {
    fn new(ct: CTerm) -> Self {
        StaticTerm(ct)
    }
}

impl Into<CTerm> for StaticTerm {
    fn into(self) -> CTerm {
        self.0
    }
}



// Convert StaticTerm to ScopedTerm
impl<'e> From<Binder<'e, StaticTerm>> for ScopedTerm<'e> {
    fn from(b: Binder<'e, StaticTerm>) -> Self {
        ScopedTerm::new(b.val.0)
    }
}



/// Convert Rust type to NIF Term
///


// pub struct Binder<'a, 'e, T:'a> {
//     env: &'e Env,
//     val: &'a T,
// }

// pub trait Bindable
//     where Self: Sized {
//     fn bind<'a, 'e>(&'a self, env: &'e Env) -> Binder<'a, 'e, Self>;
// }

// impl<T> Bindable for T
// {
//     fn bind<'a, 'e>(&'a self, env: &'e Env) -> Binder<'a, 'e, Self> {
//         Binder{env: env, val: self}
//     }
// }




pub struct Binder<'f, T> {
    pub env: &'f Env,  // FIXME remove pub
    pub val: T,
}

pub trait Bind
    where Self: Sized
{
    fn bind<'e>(self, env: &'e Env) -> Binder<'e, Self> {
        Binder{env: env, val: self}
    }
}

impl<'a> Bind for ScopedTerm<'a> {}

impl<'e> From<Binder<'e, ScopedTerm<'e>>> for ScopedTerm<'e> {
    fn from(b: Binder<'e, ScopedTerm<'e>>) -> Self {
        b.val
    }
}

impl<'e> TryFrom<Binder<'e, ScopedTerm<'e>>> for ScopedTerm<'e> {
    type Err = Error;
    fn try_from(b: Binder<ScopedTerm<'e>>) -> Result<Self> {
        Ok(b.val)
    }
}



// This is just https://doc.rust-lang.org/std/convert/trait.TryFrom.html
// It is repeated here because:
//   1. At this time the trait is marked unstable.
//   2. External traits cannot be implemented for tuples (https://github.com/rust-lang/rust/issues/31682)
//
// Once both these issues are corrected then this can be replaced with std::convert::TryFrom
//

pub trait TryFrom<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Err;

    /// Performs the conversion.
    fn try_from(T) -> std::result::Result<Self, Self::Err>;
}
pub trait TryInto<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Err;

    /// Performs the conversion.
    fn try_into(self) -> std::result::Result<T, Self::Err>;
}
impl<T, U> TryInto<U> for T where U: TryFrom<T> {
    type Err = U::Err;

    fn try_into(self) -> std::result::Result<U, U::Err> {
        U::try_from(self)
    }
}


/// An owned, process independent environment
///
/// RAII wrapper for process independent environments.
// #[derive(Debug)]
// pub struct OwnedEnv {
//  env: *mut Env,
//  marker: std::marker::PhantomData<Env>,
// }

// impl OwnedEnv {
//  pub fn new() -> OwnedEnv {
//      OwnedEnv{
//          env: unsafe{ ens::enif_alloc_env() },
//          marker: std::marker::PhantomData,
//      }
//  }

//  // pub fn get_env(&mut self) -> &mut Env {
//  //  unsafe{ &mut *self.env }
//  // }

// }

// impl AsMut<Env> for OwnedEnv {
//  fn as_mut(&mut self) -> &mut Env {
//      unsafe{ &mut *self.env }
//  }
// }

// impl Deref for OwnedEnv {
//     type Target = Env;

//     fn deref(& self) -> & Env {
//         unsafe{ &*self.env }
//     }
// }

// impl DerefMut for OwnedEnv {
//     fn deref_mut(&mut self) -> &mut Env {
//         unsafe{ &mut *self.env }
//     }
// }


// impl Drop for OwnedEnv {
//  fn drop(&mut self) {
//         unsafe{ ens::enif_free_env(self.env) };
//     }
// }

// /// An environment that has been invalidated by a `send()`.
// ///
// /// The environment may be `clear()`ed to yield another `OwnedEnv`,
// /// or just dropped to destroy the enviroment.
// #[derive(Debug)]
// pub struct InvalidEnv {
//  invalid: OwnedEnv,
// }

// impl InvalidEnv {
//  fn new(env: OwnedEnv) -> InvalidEnv {
//      InvalidEnv{ invalid: env }
//  }
//  pub fn clear(self) -> OwnedEnv {
//      unsafe{ ens::enif_clear_env(self.invalid.env); }
//      self.invalid
//  }
// }



// pub enum SendResult {
//  Ok(InvalidEnv),
//  Err(OwnedEnv)
// }

// impl SendResult {
//  pub fn unwrap(self) -> InvalidEnv {
//      match self {
//          SendResult::Ok(env) => env,
//          SendResult::Err(_) => panic!("SendResult unwrap failed")
//      }
//  }
// }


// pub unsafe fn send_from_thread(to_pid: &Pid, msg_env: OwnedEnv, msg:CTerm) -> result::Result<InvalidEnv, OwnedEnv> {
//  send_main(std::ptr::null_mut(), to_pid, msg_env, msg)
// }

// pub unsafe fn send_from_process(env: &mut Env, to_pid: &Pid, msg_env: OwnedEnv, msg:CTerm) -> result::Result<InvalidEnv, OwnedEnv> {
//  send_main(env, to_pid, msg_env, msg)
// }

// fn send_main(env: *mut Env, to_pid: &Pid, mut msg_env: OwnedEnv, msg:CTerm) -> result::Result<InvalidEnv, OwnedEnv> {
//  unsafe{
//      match ens::enif_send(env, &*to_pid, msg_env.as_mut(), msg) {
//          0 => Err(msg_env),
//          _ => Ok(InvalidEnv::new(msg_env))
//      }
//  }
// }



// // Get pid of current process.
// // Underlying nif implementation will provide an invalid PID if the
// // provided environment is process independent.
// pub fn selfpid(env:&mut Env) -> Pid {
//  unsafe {
//      let mut pid:Pid = std::mem::uninitialized();
//      ens::enif_self(env, &mut pid);
//      pid
//  }
// }


// /// Send message and free sending environment.
// pub fn send_from_process(env:&Env, pid:&Pid, msg_env:EnvPtr, msg:Term) {
//  send_from_process_main(&env, &pid, msg_env, msg);
// }


// /// Send message and recycle environment (enif_clear_env())
// pub fn send_from_process_recycle(env:&Env, pid:&Pid, msg_env:EnvPtr, msg:Term) -> EnvPtr {
//  let msg_env2 = send_from_process_main(&pid, msg_env, msg);
//  unsafe {enif_clear_env(msg_env2);}
//  msg_env
// }

// fn send_from_process_main(env:&Env, pid:&Pid, msg_env:EnvPtr, msg:Term) -> EnvPtr {
//  enif_send(env, &pid, &msg_env, msg);
//  msg_env
// }






#[derive(Debug, Copy, Clone)]
pub enum Error {
    Badarg,
}

// impl error::Error for NifError {
//  fn description(&self) -> &str {
//      match *self {
//          NifError::Badarg => "bad argument",
//      }
//  }
// }


// stuct Badarg;

// impl error::Error for Badarg {
//  fn description(&self) -> &str {
//      "bad argument"
//  }
// }


pub type Result<T> = result::Result<T, Error>;






//  // pub unsafe fn unchecked_from_term(term: Term) -> Atom {
//  //  Atom {
//  //      term: term.get_unchecked(),
//  //  }
//  // }
// }

// Atoms are not environment bound, so don't actually check environment for Atoms.
// impl DontCheckEnv for Atom {}

// impl AsTerm<CTerm> for Atom {
//  fn as_term(&self, _env: &mut Env) -> CTerm {
//      self.0
//  }
// }

// impl FromTerm<CTerm> for Atom {
//  fn from_term(env: &mut Env, term: CTerm) -> Result<Self> {
//      match unsafe{ ens::enif_is_atom(env, term) } {
//          0 => Err(Error::Badarg),
//          _ => Ok(Atom(term)),
//      }
//  }
// }

// // Atoms are not environment bound, so don't actually check environment for Atoms.
// impl FromTerm<CheckedTerm> for Atom {
//  fn from_term(env:&mut Env, term: CheckedTerm) -> Result<Self> {
//      let cterm = term.get_unchecked();
//      FromTerm::from_term(env, cterm)
//  }
// }


////////////
// Basic types



macro_rules! impl_simple_conversion {
    ($datatype:ty, $to:expr, $from:expr) => (

        impl Bind for $datatype {}

        impl<'e> From<Binder<'e, $datatype>> for ScopedTerm<'e> {
            fn from(b: Binder<$datatype>) -> Self {  // 'e elided on input and output
                ScopedTerm::new(  unsafe{$to(std::mem::transmute(b.env), b.val)}  )
            }
        }

        impl<'a, 'e> TryFrom<Binder<'e, ScopedTerm<'a>>> for $datatype {
            type Err = Error;
            fn try_from(b: Binder<ScopedTerm>) -> Result<Self> { // 'e elided on input, no output lifetime
                let mut result = unsafe {std::mem::uninitialized()};
                match unsafe{$from(std::mem::transmute(b.env), b.val.into(), &mut result)} {
                    0 => Err(Error::Badarg),
                    _ => Ok(result),
                }
            }
        }
    );
}


impl_simple_conversion!(ens::c_int,    ens::enif_make_int,    ens::enif_get_int);
impl_simple_conversion!(ens::c_uint,   ens::enif_make_uint,   ens::enif_get_uint);
impl_simple_conversion!(ens::c_double, ens::enif_make_double, ens::enif_get_double);
impl_simple_conversion!(i64,           ens::enif_make_int64,  ens::enif_get_int64);
impl_simple_conversion!(u64,           ens::enif_make_uint64, ens::enif_get_uint64);



////////////
// CTerm

// impl AsTerm<CTerm> for CTerm {
//  fn as_term(&self, _env: &mut Env) -> CTerm {
//      *self
//  }
// }
// impl FromTerm<CTerm> for CTerm {
//  fn from_term(_env: &mut Env, term: CTerm) -> Result<Self> {
//      Ok(term)
//  }
// }


////////////
// Erlang tuple as slice

// impl<'a> AsTerm<CTerm> for &'a [CTerm] {
//  fn as_term(&self, env: &mut Env) -> CTerm {
//      unsafe {
//          enif_make_tuple_from_array(
//              env,
//              self.as_ptr(),
//              self.len() as c_uint)
//      }
//  }
// }

// impl<'a> FromTerm<CTerm> for &'a [CTerm] {
//  fn from_term(env:&mut Env, term: CTerm) -> Result<Self> {
//      unsafe {
//          let mut arity:c_int = std::mem::uninitialized();
//          let mut array:*const CTerm = std::mem::uninitialized();
//          match ens::enif_get_tuple(env, term, &mut arity, &mut array) {
//              0 => Err(Error::Badarg),
//              _ => Ok(std::slice::from_raw_parts(array, arity as usize)),
//          }
//      }
//  }
// }







mod tuple;
pub use tuple::*;

mod binary;
pub use binary::*;

mod atom;
pub use atom::*;

mod resource;
pub use resource::*;

mod privdata;
pub use privdata::*;



// List Iterator

// struct<'a> ListDecoder {
//  env: &'a Env,
//  tail: ERL_NIF_TERM,
// }

// impl ListDecoder {
//  fn new<'a>(env:&'a mut Env, term: Term<'a>) -> Result<ListDecoder<'a>> {
//      unsafe {
//          match enif_is_list(transmute(env), transmute(term)) {
//              0 => Err(Error::Badarg),
//              _ => Ok(ListDecoder{env: env, tail: transmute(term)})
//          }
//      }
//  }
// }

// impl Iterator<'a> for ListDecoder<'a> {
//  type Item = Term<'a>;
//  fn next(&mut self) -> Option<Self::Item> {
//      unsafe {
//          let mut head = uninitialized();
//          let mut newtail = uninitialized();
//          match enif_get_list_cell(self.env, self.tail, &mut head, &mut newtail) {
//              0 => None,
//              _ => {
//                  self.tail = newtail;
//                  Some(transmute(head))
//              },
//          }
//      }
//  }
// }






// pub type RusterFnType = fn(env:&mut Env, args:TermSlice) -> Result<Term>;



// // Checked version
// #[cfg(debug_assertions)]
// #[inline]
// pub fn ruster_fn_wrapper(env: *mut Env,
//                             argc: c_int,
//                          args: *const CTerm,
//                          ruster_fn: RusterFnType,
//                          ) -> CTerm {
//     unsafe {
//      let cterms = std::slice::from_raw_parts(args, argc as usize);
//      //let terms:Vec<CheckedTerm> = cterms.iter().map(|term| CheckedTerm::new(env,*term)).collect();
//      let terms = Checked::new(env, cterms);
//         match ruster_fn(&mut *env, terms) {
//             Ok(rterm) => rterm.get(env),
//             _         => ens::enif_make_badarg(env),
//         }
//     }
// }

// // Unchecked version
// #[cfg(not(debug_assertions))]
// pub fn ruster_fn_wrapper(env: *mut Env,
//                             argc: c_int,
//                          args: *const CTerm,
//                          ruster_fn: RusterFnType,
//                          ) -> CTerm {
//     unsafe {
//      let terms = std::slice::from_raw_parts(args as *const Term, argc as usize);
//         match ruster_fn(&mut *env, terms) {
//             Ok(rterm) => rterm,
//             _         => ens::enif_make_badarg(env),
//         }
//     }
// }




// #[macro_export]
// macro_rules! nif_wrapper {
//  ($wrapper:ident, $wrappee:ident) => (
//      extern "C" fn $wrapper(env: *mut $crate::Env,
//                                argc: $crate::erlang_nif_sys::c_int,
//                                args: *const $crate::CTerm) -> $crate::CTerm
//      {
//          $crate::ruster_fn_wrapper(env, argc, args, $wrappee)
//      }
//  )
// }


#[macro_export]
macro_rules! ruster_fn {
    ($f:expr) => ( {
                use $crate::erlang_nif_sys as ens;
                |env: *mut ens::ErlNifEnv, argc: ens::c_int, args: *const ens::ERL_NIF_TERM| -> ens::ERL_NIF_TERM {
                    match $f(&*(env as *const Env), std::slice::from_raw_parts(std::mem::transmute(args), argc as usize)) {
                        Ok(rterm) => rterm.into(),
                        _         => ens::enif_make_badarg(env),
                    }
                }
            }
    );
}


// from experiments: note use of hrtb
// fn execute_nif<F>(f: F)
//     where F: for<'a>  Fn(&'a Env, &'a[Term<'a>]) -> Term<'a> {

//     let env = Env{};
//     let terms = [Term{ marker: PhantomData }, Term{ marker: PhantomData }];

//     f(&env, &terms);
//     //println!("got {:?}", r);
// }


// #[macro_export]
// macro_rules! nif_wrapper {
//  ($wrapper:ident, $wrappee:ident) => (
//      extern "C" fn $wrapper(env: *mut erlang_nif_sys::ErlNifEnv,
//                                argc: erlang_nif_sys::c_int,
//                                args: *const erlang_nif_sys::ERL_NIF_TERM) -> erlang_nif_sys::ERL_NIF_TERM {
//          unsafe {
//              let cterms = std::slice::from_raw_parts(args as *const Term, argc as usize);
//              let terms:Vec<Term> = cterms.map(|term| new(env,term)).collect();
//              match $wrappee(env, terms) {
//                  Ok(rterm) => rterm.get(),
//                  _         => erlang_nif_sys::enif_make_badarg(env),
//              }
//          }
//      }
//  )
// }



// unchecked
// #[macro_export]
// macro_rules! nif_wrapper {
//  ($wrapper:ident, $wrappee:ident) => (
//      extern "C" fn $wrapper(env: *mut erlang_nif_sys::ErlNifEnv,
//                                argc: erlang_nif_sys::c_int,
//                                args: *const erlang_nif_sys::ERL_NIF_TERM) -> erlang_nif_sys::ERL_NIF_TERM {
//          unsafe {
//              match $wrappee(std::mem::transmute(env), std::slice::from_raw_parts(args as *const Term, argc as usize)) {
//                  Ok(x) => std::mem::transmute(x),
//                  _     => erlang_nif_sys::enif_make_badarg(env),
//              }
//          }
//      }
//  )
// }


// #[macro_export]
// macro_rules! ruster_init {
//     ( $($things:tt)* ) => ( nif_init!($($things:tt)*) )
// }
