
use super::*;



pub trait PrivData: Sized {
    fn load<'e>(env: &'e ProcEnv, load_info: ScopedTerm<'e>) -> Self;

    //fn upgrade<T>(env: &ProcEnv, olddata: T, load_info: Term<'a>) -> Self {
    // 	olddata.into()
    // }
    fn unload(self, _env: &ProcEnv) {}

    fn get(env: &ProcEnv) -> &'static Self {
        unsafe { &*(ens::enif_priv_data(env.into_ptr()) as *mut Self) }
    }
}


struct DummyPrivType;
impl PrivData for DummyPrivType {
    fn load<'e>(_env: &'e ProcEnv, _load_info: ScopedTerm<'e>) -> Self {
        DummyPrivType
    }
}



pub fn load<T: PrivData>(penv: *mut ens::ErlNifEnv,
                         priv_data: *mut *mut ens::c_void,
                         load_info: ens::ERL_NIF_TERM)
                         -> ens::c_int {
    let env = ProcEnv::from_ptr(penv);
    //init_static_atom_data(env, &atom::static_atom_strings);
    init_ato(penv);
    let data: Box<T> = Box::new(PrivData::load(env, load_info.into()));
    unsafe {
        *priv_data = Box::into_raw(data) as *mut ens::c_void;
    }
    0
}

pub fn unload<T: PrivData>(penv: *mut ens::ErlNifEnv, priv_data: *mut ens::c_void) {

    let data: Box<T> = unsafe { Box::from_raw(priv_data as *mut T) };
    data.unload(ProcEnv::from_ptr(penv));
}
