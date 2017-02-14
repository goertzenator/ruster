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


# Safety Mechanisms




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

/// Trait for dealing with underlying raw NIF environment pointers.
pub trait Env: Sized {
    fn as_api_ptr(&self) -> *mut erlang_nif_sys::ErlNifEnv {
        self as *const Self as *const erlang_nif_sys::ErlNifEnv as *mut erlang_nif_sys::ErlNifEnv
    }

    fn from_api_ptr<'a>(penv: *mut erlang_nif_sys::ErlNifEnv) -> &'a Self {
        unsafe { &*(penv as *mut Self) }
    }
}



/// Object that can send message to other processes.
///
/// Sender is created by thread_sender() and ProcEnv::sender().
/// It contains reusable state, so it is more efficient to use
/// a single Sender many times than to create a new Sender for each
/// send.
pub struct Sender {
    context_penv: *mut ens::ErlNifEnv,
    dirty_penv: Option<*mut ens::ErlNifEnv>,
}

impl Sender {
    /// Create Sender from ProcEnv
    ///
    /// The Sender instance can send messages to other processes.
    pub fn from_env(env: &ProcEnv) -> Sender {
        Sender {
            context_penv: env.as_api_ptr(),
            dirty_penv: None,
        }
    }


    /// Create Sender in non-Erlang thread.
    ///
    /// This function will fail if called in any BEAM scheduler thread.
    pub fn from_thread() -> Option<Sender> {
        let thread_type = unsafe { ens::enif_thread_type() };
        match thread_type {
            ens::ERL_NIF_THR_UNDEFINED => {
                Some(Sender {
                    context_penv: std::ptr::null_mut(),
                    dirty_penv: None,
                })
            }
            _ => None,
        }
    }


    /// Send data to the designated Pid.
    pub fn send<'e, T>(&mut self, to_pid: Pid, msg: T)
        where Binder<'e, CopyEnv, T>: Into<ScopedTerm<'e>>,
              T: Bind
    {
        unsafe {
            // get clean msg_penv
            let msg_penv = if self.dirty_penv.is_none() {
                let penv = ens::enif_alloc_env();
                self.dirty_penv = Some(penv); // not dirty just yet, but will be after enif_send()
                penv
            } else {
                let penv = self.dirty_penv.unwrap();
                ens::enif_clear_env(penv);
                penv
            };

            // use CopyEnv to ensure any term inputs are deep-copied.
            let msg_term: ScopedTerm = msg.bind(CopyEnv::from_api_ptr(msg_penv)).into();
            ens::enif_send(self.context_penv, &(to_pid.0), msg_penv, msg_term.into());
        }
    }
}

impl Drop for Sender {
    fn drop(&mut self) {
        if let Some(penv) = self.dirty_penv {
            unsafe { ens::enif_free_env(penv) };
        }
    }
}


/// Environment type passed to user NIF functions.
///
/// `ProcEnv`s may send messages via the `SendMsg` trait.
pub struct ProcEnv;
impl Env for ProcEnv {}


/// Environment type representing process-independent environments.
///
/// Similar to `ProcEnv`, but may *not* send messages.
pub struct NonProcEnv;
impl Env for NonProcEnv {}

/// Environment type for messages and MobileTerm constructors.
///
/// Similar to NonProcEnv except that ScopedTerm instances are
/// deep-copied (enif_make_copy) into the bound environment.
pub struct CopyEnv;
impl Env for CopyEnv {}



/// A term with enclosing environment that can be live beyond a ProcEnv scope.
///
///
///
pub struct MobileTerm {
    penv: *mut ens::ErlNifEnv,
    cterm: ens::ERL_NIF_TERM,
}

impl MobileTerm {
    pub fn new<'e, T>(data: T) -> MobileTerm
        where Binder<'e, CopyEnv, T>: Into<ScopedTerm<'e>>,
              T: Bind
    {
        let penv = unsafe { ens::enif_alloc_env() };
        let term = data.bind(CopyEnv::from_api_ptr(penv)).into();
        MobileTerm {
            penv: penv,
            cterm: term.into(),
        }
    }

    /// Access and possibly mutate the underlying ScopedTerm.
    ///
    /// This function is unsafe because the user must take care to not
    /// allow terms from other scopes to leak in, and must not allow
    /// this term to leak out.
    pub unsafe fn access<F, T>(&mut self, f: F) -> T
        where F: Fn(&NonProcEnv, &mut ScopedTerm) -> T
    {
        f(NonProcEnv::from_api_ptr(self.penv),
          std::mem::transmute(&mut self.cterm))
    }
}

impl Drop for MobileTerm {
    fn drop(&mut self) {
        unsafe {
            ens::enif_free_env(self.penv);
        }
    }
}







/// Convenience name for low level term type.
pub use erlang_nif_sys::ERL_NIF_TERM as CTerm;





// #[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
// pub struct Term<'a> {
//     term: CTerm,
//     _env: std::marker::PhantomData<&'a Env>,
// }

#[derive(Copy, Clone)]
pub struct ScopedTerm<'a>(CTerm, std::marker::PhantomData<&'a ()>);

#[derive(Copy, Clone)]
pub struct StaticTerm(CTerm);


pub const UNINITIALIZED_STATIC_TERM: StaticTerm = StaticTerm(0);


use std::result;



impl<'a> ScopedTerm<'a> {
    fn new(ct: CTerm) -> Self {
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



// StaticTerm can always be directly converted to ScopedTerm
impl<'e, E: Env> From<Binder<'e, E, StaticTerm>> for ScopedTerm<'e> {
    fn from(b: Binder<E, StaticTerm>) -> Self {
        ScopedTerm::new(b.val.0)
    }
}




/// Temporary container type for attaching an environment to data.
///
/// Used in conjunction with conversions.
pub struct Binder<'e, E: Env + 'e, T> {
    env: &'e E,
    val: T,
}

/// Trait for attaching an environment to data.  Use in conjunction with conversions.
///
/// Used in conjunction with conversions.
pub trait Bind
    where Self: Sized
{
    fn bind<'e, E: Env>(self, env: &'e E) -> Binder<E, Self> {
        Binder {
            env: env,
            val: self,
        }
    }
}

impl<'a> Bind for ScopedTerm<'a> {}

impl<'e> From<Binder<'e, ProcEnv, ScopedTerm<'e>>> for ScopedTerm<'e> {
    fn from(b: Binder<ProcEnv, ScopedTerm<'e>>) -> Self {
        b.val
    }
}

impl<'e> TryFrom<Binder<'e, ProcEnv, ScopedTerm<'e>>> for ScopedTerm<'e> {
    type Err = Error;
    fn try_from(b: Binder<ProcEnv, ScopedTerm<'e>>) -> Result<Self> {
        Ok(b.val)
    }
}



// This is just https://doc.rust-lang.org/std/convert/trait.TryFrom.html
// It is repeated here because:
//   1. At this time the trait is marked unstable.
//   2. External traits cannot be implemented for tuples (https://github.com/rust-lang/rust/issues/31682)
//
// Once both these issues are resolved then this can be replaced with std::convert::TryFrom
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
impl<T, U> TryInto<U> for T
    where U: TryFrom<T>
{
    type Err = U::Err;

    fn try_into(self) -> std::result::Result<U, U::Err> {
        U::try_from(self)
    }
}





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








#[derive(Copy, Clone)]
pub struct Pid(erlang_nif_sys::ErlNifPid);

impl Pid {
    /// Get Pid of calling process
    pub fn from_procenv(env: &ProcEnv) -> Pid {
        unsafe {
            let mut pid = std::mem::uninitialized();
            ens::enif_self(env.as_api_ptr(), &mut pid); // won't fail; ProcEnv gaurantees valid Env type.
            Pid(pid)
        }
    }

    pub fn is_alive<E: Env>(&self, env: E) -> bool {
        unsafe { 0 != ens::enif_is_process_alive(env.as_api_ptr(), &self.0) }
    }
}

impl Bind for Pid {}

impl<'e, E: Env> From<Binder<'e, E, Pid>> for ScopedTerm<'e> {
    fn from(b: Binder<E, Pid>) -> Self {
        let env = b.env;
        let pid = b.val;
        ScopedTerm::new(unsafe { ens::enif_make_pid(env.as_api_ptr(), &(pid.0)) })
    }
}

impl<'e, E: Env> TryFrom<Binder<'e, E, ScopedTerm<'e>>> for Pid {
    type Err = Error;
    fn try_from(b: Binder<E, ScopedTerm>) -> Result<Self> {
        let env = b.env;
        let term = b.val;
        unsafe {
            let mut pid = std::mem::uninitialized();
            match ens::enif_get_local_pid(env.as_api_ptr(), term.into(), &mut pid) {
                0 => Err(Error::Badarg),
                _ => Ok(Pid(pid)),
            }
        }
    }
}


////////////
// Basic types

macro_rules! impl_simple_conversion {
    ($datatype:ty, $to:expr, $from:expr) => (

        impl Bind for $datatype {}

        // impl<'e> EnvFrom<$datatype> for ScopedTerm<'e> {
        //     fn envfrom<E:Env>(v: $datatype, env: &'e E) -> Self {  // 'e elided on input and output
        //         ScopedTerm::new(  unsafe{$to(b.env.as_api_ptr(), b.val)}  )
        //     }
        // }

        impl<'e, E:Env> From<Binder<'e, E, $datatype>> for ScopedTerm<'e> {
            fn from(b: Binder<E, $datatype>) -> Self {  // 'e elided on input and output
                ScopedTerm::new(  unsafe{$to(b.env.as_api_ptr(), b.val)}  )
            }
        }

        impl<'a, 'e, E:Env> TryFrom<Binder<'e, E, ScopedTerm<'a>>> for $datatype {
            type Err = Error;
            fn try_from(b: Binder<E, ScopedTerm>) -> Result<Self> { // 'e elided on input, no output lifetime
                let mut result = unsafe {std::mem::uninitialized()};
                match unsafe{$from(b.env.as_api_ptr(), b.val.into(), &mut result)} {
                    0 => Err(Error::Badarg),
                    _ => Ok(result),
                }
            }
        }
    );
}


impl_simple_conversion!(ens::c_int, ens::enif_make_int, ens::enif_get_int);
impl_simple_conversion!(ens::c_uint, ens::enif_make_uint, ens::enif_get_uint);
impl_simple_conversion!(ens::c_double, ens::enif_make_double, ens::enif_get_double);
impl_simple_conversion!(i64, ens::enif_make_int64, ens::enif_get_int64);
impl_simple_conversion!(u64, ens::enif_make_uint64, ens::enif_get_uint64);










mod tuple;
pub use tuple::*;


// mod atom;
// pub use atom::*;

// mod binary;
// pub use binary::*;

// mod resource;
// pub use resource::*;

// mod privdata;
// pub use privdata::*;



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



#[macro_export]
macro_rules! ruster_fn {
    ($f:expr) => ( {
                use $crate::erlang_nif_sys as ens;
                |env: *mut ens::ErlNifEnv, argc: ens::c_int, args: *const ens::ERL_NIF_TERM| -> ens::ERL_NIF_TERM {
                    match $f(&*(env as *const $crate::ProcEnv), std::slice::from_raw_parts(std::mem::transmute(args), argc as usize)) {
                        Ok(rterm) => rterm.into(),
                        _         => ens::enif_make_badarg(env),
                    }
                }
            }
    );
}
