
use super::*;

use std::{ptr, mem};
use std::ops::{Deref, DerefMut, Drop};
use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::fmt::{Debug, Display, self};
use std::sync::{Once, RwLock, ONCE_INIT};
use std::collections::{HashMap};
use std::any::{Any, TypeId};



type ResourceLut = HashMap<TypeId, *const ErlNifResourceType>;
static mut resource_lut: Option<&'static RwLock<ResourceLut>> = None;
static RESOURCE_LUT_INIT: Once = ONCE_INIT;

/// Declare a type that can be used as an Erlang Resource
///
/// This function is marked unsafe because it must be used only from `load()`.

pub unsafe fn open_resource_type<T: Any>(env: &mut Env, name: &str) -> Result<()> {
    let c_name = CString::new(name).unwrap();
    let ptr = unsafe { ens::enif_open_resource_type(env, ptr::null(), c_name.as_ptr() as *const u8,
        Some(resource_destructor::<T>), ens::ErlNifResourceFlags::ERL_NIF_RT_CREATE, ptr::null_mut()) };
    if ptr.is_null() {
        return Err(Error::Badarg);
    }
    get_resource_lut().write().unwrap().deref_mut().insert(TypeId::of::<T>(), ptr);

    Ok(())
}

fn get_resource_type<T: Any>() -> Option<*const ErlNifResourceType> {
    let lut = get_resource_lut().read().unwrap();
    lut.get(&TypeId::of::<T>()).map(|x|*x)
}


extern "C" fn resource_destructor<T>(_env: *mut ErlNifEnv, object: *mut c_void) {
    unsafe {
        ptr::drop_in_place::<T>(object as *mut T)
    }
}

fn get_resource_lut() -> &'static RwLock<ResourceLut> {
    unsafe {
        RESOURCE_LUT_INIT.call_once(|| {
            let resbox = Box::new(RwLock::new(ResourceLut::new()));
            resource_lut = Box::into_raw(resbox).as_ref()
        });
        resource_lut.unwrap()
    }
}


/// A ref counting pointer to an Erlang resource
///
/// A pointer
pub struct ResourcePtr<T>
    where T: Any {
    ptr: *const T,
    _marker: PhantomData<T>,
}

impl<T> ResourcePtr<T>
    where T: Any {

    /// Create a new resource.
    ///
    /// The type of resource created must have been previously registered with `open_resource_type`
    /// otherwise this call will panic.  `ResourcePtr`s may live beyond the scope of any environment
    pub fn new(instance: T) -> ResourcePtr<T> {
        unsafe {
            let resource_type = get_resource_type::<T>().unwrap();
            let ptr = enif_alloc_resource(resource_type, mem::size_of::<T>()) as *mut T;

            ptr::write(ptr, instance); // Move instance into allocated memory.
            // Does the move get optimized away?  If not, consider placement new when it becomes stable.

            ResourcePtr{ptr: ptr, _marker: PhantomData}
        }
    }
}

impl<T> Deref for ResourcePtr<T>
    where T: Any {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

impl<T> Drop for ResourcePtr<T>
    where T: Any {
    fn drop(&mut self) {
        unsafe {
            enif_release_resource(self.ptr as *const c_void)
        }
    }
}

impl<T> Clone for ResourcePtr<T>
    where T: Any {
    fn clone(&self) -> Self {
        unsafe {
            enif_keep_resource(self.ptr as *const c_void);
        }
        ResourcePtr{ptr: self.ptr, _marker: PhantomData}
    }
}

impl<T> Debug for ResourcePtr<T> where T: Debug + Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl<T> Display for ResourcePtr<T> where T: Display + Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}




impl<T> AsTerm<CTerm> for ResourcePtr<T>
    where T: Any {
    fn as_term(&self, env: &mut Env) -> CTerm {
        unsafe {
            enif_make_resource(env, self.ptr as *const c_void)
        }
    }
}

impl<T> FromTerm<CTerm> for ResourcePtr<T>
    where T: Any {
    fn from_term(env: &mut Env, term: CTerm) -> Result<Self> {
        unsafe {
            let mut ptr = mem::uninitialized();
            let resource_type = try!(
                get_resource_type::<T>()
                .ok_or(Error::Badarg)
            );

            match enif_get_resource(env, term, resource_type, &mut ptr) {
                0 => Err(Error::Badarg),
                _ => {
                    enif_keep_resource(ptr);
                    Ok(ResourcePtr{ptr: ptr as *const T, _marker: PhantomData})
                }
            }
        }
    }
}


/// Wrapper type for decoding resources as references
///
/// Bare references conflict with other `FromTerm` `impl`s,
/// therefore a wrapper type is required.  `ResourceRefs`
/// do not affect the refcount.  They are therefore
/// cheaper than constructing a full `ResourcePtr` but
/// may not be used outside of the constructing
/// environment.
struct ResourceRef<'a, T: 'a>(&'a T);

// conflicts with every other FromTerm impl?
// would borrow env until ref descopes.  problematic
// What is the real connection with with env?
impl<'a, T> FromTerm<CTerm> for ResourceRef<'a, T>
    where T: Any {
    fn from_term(env: &mut Env, term: CTerm) -> Result<Self> {
        unsafe {
            let mut ptr = mem::uninitialized();
            let resource_type = try!(
                get_resource_type::<T>()
                .ok_or(Error::Badarg)
            );

            match enif_get_resource(env, term, resource_type, &mut ptr) {
                0 => Err(Error::Badarg),
                _ => Ok(ResourceRef(&*(ptr as *const T))),
            }
        }
    }
}

