
use super::*;

use std::{ptr, mem};
use std::ops::{Deref, Drop};
use std::marker::PhantomData;
use std::fmt::{self, Debug, Display};
use std::any::{Any, TypeId};


//
// "Any" Trait Object (ATO) Resource
//
// The ATO Resource is a generic resource type that allows any Rust type to be used with a single
// enif_open_resource_type() registration.  The "Any" trait object is emulated with
// a pseudo-VMT (pseudo because it is a function and not a table)

static mut ATO_RESOURCE: *const ens::ErlNifResourceType = 0 as *const ens::ErlNifResourceType;


struct ATOWrapper<T: 'static> {
    vmtf: fn(VMTParams),
    data: T,
}

enum VMTParams {
    Typeid(*mut TypeId),
    Dtor(*mut ens::ErlNifEnv, *mut ens::c_void),
}

fn vmt_function<T: 'static>(params: VMTParams) {
    match params {
        VMTParams::Typeid(id) => unsafe { *id = TypeId::of::<T>() },
        VMTParams::Dtor(_env, obj) => unsafe { ptr::drop_in_place(obj as *mut ATOWrapper<T>) },
    }
}

impl<T: 'static> ATOWrapper<T> {
    fn new(data: T) -> Self {
        ATOWrapper {
            vmtf: vmt_function::<T>,
            data: data,
        }
    }

    fn is<U: 'static>(&self) -> bool {
        unsafe {
            let mut typeid = mem::uninitialized();
            (self.vmtf)(VMTParams::Typeid(&mut typeid));
            typeid == TypeId::of::<U>()
        }
    }
}

extern "C" fn ato_destructor(env: *mut ens::ErlNifEnv, objp: *mut ens::c_void) {
    unsafe {
        let obj: &ATOWrapper<()> = &*(objp as *const ATOWrapper<()>);
        (obj.vmtf)(VMTParams::Dtor(env, objp));
    }
}

pub fn init_ato(env: *mut ens::ErlNifEnv) {
    unsafe {
        ATO_RESOURCE = ens::enif_open_resource_type(env,
                                                    ptr::null(),
                                                    b"Rust Any trait object\0" as *const u8,
                                                    Some(ato_destructor),
                                                    ens::ErlNifResourceFlags::ERL_NIF_RT_CREATE,
                                                    ptr::null_mut());
    }
}

// when enif_select() lands...
// pub trait ErlangSelect {}



/// A ref counting pointer to an Erlang resource
///
/// A pointer
pub struct Resource<T: 'static> {
    ptr: *const ATOWrapper<T>,
    _marker: PhantomData<T>,
}

impl<T: 'static> Bind for Resource<T> {}
impl<'a, T: 'static> Bind for &'a Resource<T> {}

impl<T: 'static> Resource<T> {
    /// Create a new resource.
    ///
    /// The type of resource created must have been previously registered with `open_resource_type`
    /// otherwise this call will panic.  `ResourcePtr`s may live beyond the scope of any environment
    pub fn new(data: T) -> Resource<T> {
        unsafe {
            let instance: ATOWrapper<T> = ATOWrapper::new(data);
            let objp = ens::enif_alloc_resource(ATO_RESOURCE, mem::size_of_val(&instance)) as
                       *mut ATOWrapper<T>;

            ptr::write(objp, instance); // Move instance into allocated memory.
            // Does the move get optimized away?  If not, consider placement new when it becomes stable.

            Resource {
                ptr: objp,
                _marker: PhantomData,
            }
        }
    }

    unsafe fn from_objp(objp: *const ens::c_void) -> Option<Resource<T>> {
        let ato_wrapper = &*(objp as *const ATOWrapper<T>);
        match ato_wrapper.is::<T>() {
            true => {
                ens::enif_keep_resource(objp);
                Some(Resource {
                    ptr: objp as *const ATOWrapper<T>,
                    _marker: PhantomData,
                })
            }
            false => None,
        }
    }

    unsafe fn ref_from_objp<'a>(objp: *const ens::c_void) -> Option<&'a T> {
        let ato_wrapper = &*(objp as *const ATOWrapper<T>);
        match ato_wrapper.is::<T>() {
            true => Some(&ato_wrapper.data),
            false => None,
        }
    }
}

// when enif_select() lands...
// impl<T: Select> Resource<T> {
//     pub fn new_select(data: T) -> Resource<T> {..}
// }


impl<T> Deref for Resource<T>
    where T: Any
{
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &(*self.ptr).data }
    }
}

impl<T: 'static> Drop for Resource<T> {
    fn drop(&mut self) {
        unsafe { ens::enif_release_resource(self.ptr as *const ens::c_void) }
    }
}

impl<T: 'static> Clone for Resource<T> {
    fn clone(&self) -> Self {
        unsafe {
            ens::enif_keep_resource(self.ptr as *const ens::c_void);
        }
        Resource {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> Debug for Resource<T>
    where T: Debug + Any
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl<T> Display for Resource<T>
    where T: Display + Any
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}

//
// Resource conversions
//

impl<'e, E: Env, T: 'static> TryFrom<Binder<'e, E, ScopedTerm<'e>>> for Resource<T> {
    type Err = Error;
    fn try_from(b: Binder<'e, E, ScopedTerm>) -> Result<Self> {
        unsafe {
            // get object pointer
            let env = b.env;
            let term = b.val;
            let mut objp = mem::uninitialized();
            if 0 == ens::enif_get_resource(env.as_api_ptr(), term.into(), ATO_RESOURCE, &mut objp) {
                return Err(Error::Badarg);
            }
            Resource::<T>::from_objp(objp).ok_or(Error::Badarg)
        }
    }
}

impl<'e, E: Env, T: 'static> TryFrom<Binder<'e, E, ScopedTerm<'e>>> for &'e T {
    type Err = Error;
    fn try_from(b: Binder<'e, E, ScopedTerm>) -> Result<Self> {
        unsafe {
            // get object pointer
            let env = b.env;
            let term = b.val;
            let mut objp = mem::uninitialized();
            if 0 == ens::enif_get_resource(env.as_api_ptr(), term.into(), ATO_RESOURCE, &mut objp) {
                return Err(Error::Badarg);
            }
            Resource::<T>::ref_from_objp(objp).ok_or(Error::Badarg)
        }
    }
}

impl<'a, 'e, E: Env, T: 'static> From<Binder<'e, E, &'a Resource<T>>> for ScopedTerm<'e> {
    fn from(b: Binder<'e, E, &Resource<T>>) -> Self {
        unsafe {
            let env = b.env;
            let res = b.val;
            ScopedTerm::new(ens::enif_make_resource(env.as_api_ptr(),
                                                    res.ptr as *const ens::c_void))
        }
    }
}

impl<'e, E: Env, T: 'static> From<Binder<'e, E, Resource<T>>> for ScopedTerm<'e> {
    fn from(b: Binder<'e, E, Resource<T>>) -> Self {
        unsafe {
            let env = b.env;
            let res = b.val;
            ScopedTerm::new(ens::enif_make_resource(env.as_api_ptr(),
                                                    res.ptr as *const ens::c_void))
        }
    }
}
