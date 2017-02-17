
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
// a pseudo-VMT (pseudo because it is a function and not a table).  This adds 1 word to
// each resource object (Erlang itself adds 4).

static mut ATO_RESOURCE: *const ens::ErlNifResourceType = 0 as *const ens::ErlNifResourceType;

// The "layout" of resource object memory
struct ATOWrapper<T: 'static> {
    vmtf: fn(VMTParams),
    data: T,
}
impl<T: 'static> ATOWrapper<T> {
    fn new(data: T) -> Self {
        ATOWrapper {
            vmtf: vmt_function::<T>,
            data: data,
        }
    }

    /// Return true if U is the same type as self.
    ///
    /// "T" is not available in this context; the TypeId
    /// must be dynamically retrieved through the vmtf.
    fn is<U: 'static>(&self) -> bool {
        unsafe {
            let mut typeid = mem::uninitialized();
            (self.vmtf)(VMTParams::Typeid(&mut typeid));
            typeid == TypeId::of::<U>()
        }
    }
}

// The "methods" available in the pseudo-VMT.
// More can be added when enif_select() lands.
enum VMTParams {
    Typeid(*mut TypeId),
    Dtor(*mut ens::ErlNifEnv, *mut ens::c_void),
}

fn vmt_function<T: 'static>(params: VMTParams) {
    match params {
        VMTParams::Typeid(id) => unsafe { *id = TypeId::of::<T>() },
        VMTParams::Dtor(_penv, obj) => unsafe { ptr::drop_in_place(obj as *mut ATOWrapper<T>) },
    }
}


// ATO Resource type registration
pub fn init_ato(penv: *mut ens::ErlNifEnv) {
    unsafe {
        ATO_RESOURCE = ens::enif_open_resource_type(penv,
                                                    ptr::null(),
                                                    b"Rust Any trait object\0" as *const u8,
                                                    Some(ato_destructor),
                                                    ens::ErlNifResourceFlags::ERL_NIF_RT_CREATE,
                                                    ptr::null_mut());
    }
}

/// ATO Resource destructor.
///
/// Use wrapper's vmt function pointer to invoke the actual type-specific destructor
extern "C" fn ato_destructor(penv: *mut ens::ErlNifEnv, objp: *mut ens::c_void) {
    unsafe {
        let obj: &ATOWrapper<()> = &*(objp as *const ATOWrapper<()>);
        (obj.vmtf)(VMTParams::Dtor(penv, objp));
    }
}






// when enif_select() lands...
// pub trait ErlangSelect {}



/// A ref counting pointer to an Erlang resource
///
/// Similar in functin to Arc.
pub struct Resource<T: 'static> {
    ptr: *const ATOWrapper<T>,
    _marker: PhantomData<T>,
}

impl<T: 'static> Resource<T> {
    /// Create a new resource.
    ///
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

    /// Convert NIF resource pointer to a Resource<T>
    ///
    /// Returns None if the underlying type is not T.
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

    /// Convert NIF resource pointer to a &T
    ///
    /// Returns None if the underlying type is not T.
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

impl<T: AsRef<[u8]>> Resource<T> {
    /// Create binary term for this resource.
    ///
    /// Resources that implement AsRef<[u8]> can be converted to a term
    /// representing a binary.  Term adds a reference count to resource
    /// which is retracted only when the binary term is garbage collected.
    /// The data the term refers to must not mutate.
    pub fn as_binary_term<E: Env>(&self, env: &E) -> ScopedTerm {
        unsafe {
            let bin: &[u8] = self.as_ref();
            ens::enif_make_resource_binary(
                env.into_ptr(),
                self.ptr as *const ens::c_void,
                bin.as_ptr() as *const ens::c_void,
                bin.len())
            .into()
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

/// Resource reference wrapper for low-cost resource retrieval.
///
/// Use for obtaining a low-cost resource reference to a resource term.
/// References cannot be used directly because they cause collisions
/// in the trait space.
pub struct ResourceRef<'e, T:'static>(&'e T);

impl<'e, T> Deref for ResourceRef<'e, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'e, E: Env, T: 'static> TryEnvFrom<ScopedTerm<'e>,E> for Resource<T> {
    type Err = Error;
    fn try_efrom(term: ScopedTerm, env: &E) -> Result<Self> {
        unsafe {
            // get object pointer
            let mut objp = mem::uninitialized();
            if 0 == ens::enif_get_resource(env.into_ptr(), term.into(), ATO_RESOURCE, &mut objp) {
                return Err(Error::Badarg);
            }
            Resource::<T>::from_objp(objp).ok_or(Error::Badarg)
        }
    }
}


//FIXME: Revisit plain reference usage when trait specialization gets better.

impl<'e, E: Env, T: 'static> TryEnvFrom<ScopedTerm<'e>,E> for ResourceRef<'e,T> {
    type Err = Error;
    fn try_efrom(term: ScopedTerm, env: &E) -> Result<Self> {
        unsafe {
            // get object pointer
            let mut objp = mem::uninitialized();
            if 0 == ens::enif_get_resource(env.into_ptr(), term.into(), ATO_RESOURCE, &mut objp) {
                return Err(Error::Badarg);
            }
            Resource::<T>::ref_from_objp(objp).ok_or(Error::Badarg).map(|x| ResourceRef(x))
        }
    }
}

impl<'a, 'e, E: Env, T: 'static> EnvFrom<&'a Resource<T>, E> for ScopedTerm<'e> {
    fn efrom(res: &'a Resource<T>, env: &E) -> Self {
        unsafe {
            ens::enif_make_resource(env.into_ptr(),
                                                    res.ptr as *const ens::c_void).into()
        }
    }
}

impl<'e, E: Env, T: 'static> EnvFrom<Resource<T>, E> for ScopedTerm<'e> {
    fn efrom(res: Resource<T>, env: &E) -> Self {
        EnvFrom::efrom(&res, env)
    }
}
