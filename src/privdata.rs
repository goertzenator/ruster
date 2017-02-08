
use super::*;




/////////////////////////////////////////////////////////////////////
// in Ruster



trait PrivData {
	fn load<'a>(env: &'a Env, load_info: Term<'a>) -> Self {
		Default::default()
	}

	//fn upgrade<T>(env: &Env, olddata: T, load_info: Term<'a>) -> Self {
	// 	olddata.into()
	// }
	fn unload(self, env: &Env) {}

}


struct PrivData<U: Default> {
	userdata: U
}

impl PrivData<U> {
	fn new() -> Self {
		PrivData{
			userdata: Default::default(),
		}
	}
}

// synthesized in user program with non-generic type

//type PrivData =  ruster::privdata::PrivData<U>;





/////////////////////////////////////////////////////////////////////
// in user lib from macros....

ruster_init!( ....
[]

{privdata: MyType,
 oldprivdata: (defaults to privdata)   ...}
	)


struct MyType {...}
impl NIFLoad for MyType {}


// synthesized in user program with non-generic type
mod ruster_module_functions {
	fn load(env: *mut ErlNifEnv,
	        priv_data: *mut *mut c_void,
	        load_info: ERL_NIF_TERM)-> c_int {
		let data: Box<MyType> = Box::new( PrivData::load(Env::from_api_ptr(penv), load_info) );
	    unsafe{ *priv_data = Box::into_raw(data) as *mut c_void; }
	    0
		}
	}

	// fn upgrade(env: *mut ErlNifEnv,
	//            priv_data: *mut *mut c_void,
	//            old_priv_data: *mut *mut c_void,
	//                       load_info: ERL_NIF_TERM) -> c_int {
	// 	let olddata: Box<T> = Box::from_raw(old_priv_data as *mut T);
	// 	let data: Box<MyType> = Box::new( NIFLoad::upgrade(Env::from_api_ptr(penv), *T, load_info) );
	//     unsafe{
	//     	*priv_data = Box::into_raw(data) as *mut c_void;
	//     	*old_priv_data = std::ptr::null_mut(); // NIFLoad::upgrade consumes oldata
	//     }
	// }

	fn unload(env: *mut ErlNifEnv,
	          priv_data: *mut c_void) {

		let data: Box<MyType> = Box::from_raw(priv_data as *mut MyType);
		data.unload(Env::from_api_ptr(penv), load_info)

	    unsafe{ *priv_data = Box::into_raw(data) as *mut c_void; }
	}
}

// synthesized in user program with non-generic type
fn priv_data(env: &Env) -> &'static MyType {
	unsafe {
		enif_priv_data(env.as_api_ptr()) as &c_void as &MyType;
	}
}


