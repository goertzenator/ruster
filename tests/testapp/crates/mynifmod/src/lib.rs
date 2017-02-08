

// #[macro_use]
// extern crate erlang_nif_sys;


#[macro_use]
extern crate ruster;

use ruster::{Env, ScopedTerm, StaticTerm, Binary, Resource,
    StaticAtom, Binder, Error, Bind, TryInto, Result};

// use nif::{open_resource_type, new_binary, ResourcePtr, Error};
// use nif::{selfpid, send_from_process};
//use nif::erlang_nif_sys::{enif_priv_data, c_void, c_int};

use std::cell::Cell;

/// Create NIF module data and init function.
nif_init!("mynifmod", [
        // ("dynamic_atom",    0, ruster_fn!(dynamic_atom_wrapper)),
        // ("prebuilt_atom",   0, ruster_fn!(prebuilt_atom_wrapper)),
        ("int",             0, ruster_fn!(int)),
        ("add_ints1",       2, ruster_fn!(add_ints1)),
        ("tuple1",          1, ruster_fn!(tuple1)),
        ("tuple2",          4, ruster_fn!(tuple2)),
        ("tuple0",          2, ruster_fn!(tuple0)),
        ("catbin",          2, ruster_fn!(catbin)),
        ("staticatom",      0, ruster_fn!(staticatom)),
        ("mathcommand",     3, ruster_fn!(mathcommand)),
        ("makeresources",   2, ruster_fn!(makeresources)),
        ("incresources",    3, ruster_fn!(incresources)),
        ("getresources",    2, ruster_fn!(getresources)),

        // ("doubleit",        1, ruster_fn!(doubleit_wrapper)),
        // ("selfsend",        0, ruster_fn!(selfsend_wrapper)),
        // ("decode_int",      1, ruster_fn!(decode_int_wrapper)),
        // ("tuple_math",      1, ruster_fn!(tuple_math_wrapper)),
        // ("param2tuple",     2, ruster_fn!(param2tuple_wrapper)),
        // ("make_resource",   1, ruster_fn!(make_resource_wrapper)),
        // ("render_resource", 2, ruster_fn!(render_resource_wrapper)),
        // ("mutate_resource", 3, ruster_fn!(mutate_resource_wrapper)),
        // ("reverse_binary",  1, ruster_fn!(reverse_binary_wrapper)),
    ],
    {load: ruster_module_load, unload: ruster_module_unload}
);



/////////////////////////////////
// NIF Private Data


trait PrivData: Sized + Default {
    fn load<'a>(env: &'a Env, load_info: ScopedTerm<'a>) -> Self {
        Default::default()
    }

    //fn upgrade<T>(env: &Env, olddata: T, load_info: ScopedTerm<'a>) -> Self {
    //  olddata.into()
    // }
    fn unload(self, env: &Env) {}

}

// No good without trait specialization
// impl<T> PrivData for T
//     where T: Default
//     {}


#[derive(Default)]
struct MyType {}
impl PrivData for MyType {}
use ruster::erlang_nif_sys as ens;

// synthesized in user program with non-generic type

fn ruster_module_load(penv: *mut ens::ErlNifEnv,
        priv_data: *mut *mut ens::c_void,
        load_info: ens::ERL_NIF_TERM)-> ens::c_int {
    let env = Env::from_api_ptr(penv);
    let data: Box<MyType> = Box::new( PrivData::load(env, ScopedTerm::new(load_info) ));
    unsafe{ *priv_data = Box::into_raw(data) as *mut ens::c_void; }
    ruster::init_static_atom_data(env, &atom::static_atom_strings);
    ruster::init_ato(penv);
    0
}


// fn ruster_module_upgrade(env: *mut ens::ErlNifEnv,
//            priv_data: *mut *mut ens::c_void,
//            old_priv_data: *mut *mut ens::c_void,
//                       load_info: ens::ERL_NIF_TERM) -> ens::c_int {
//  let olddata: Box<T> = Box::from_raw(old_priv_data as *mut T);
//  let data: Box<MyType> = Box::new( NIFLoad::upgrade(Env::from_api_ptr(penv), *T, ScopedTerm::new(load_info)) );
//     unsafe{
//      *priv_data = Box::into_raw(data) as *mut ens::c_void;
//      *old_priv_data = std::ptr::null_mut(); // NIFLoad::upgrade consumes oldata
//     }
// }

fn ruster_module_unload(penv: *mut ens::ErlNifEnv,
          priv_data: *mut ens::c_void) {

    let data: Box<MyType> = unsafe{ Box::from_raw(priv_data as *mut MyType) };
    data.unload(Env::from_api_ptr(penv));
    ruster::destroy_static_atom_data();
}


// synthesized in user program with non-generic type
fn priv_data(env: &Env) -> &'static MyType {
    unsafe {
        &*(ens::enif_priv_data(env.as_api_ptr()) as *mut MyType)
    }
}

#[allow(non_upper_case_globals)]
pub mod atom {
    use std;
    use ruster::*;
    pub const OK: StaticAtom = StaticAtom(0);
    pub const ERROR: StaticAtom = StaticAtom(1);
    pub const ADD: StaticAtom = StaticAtom(2);
    pub const SUB: StaticAtom = StaticAtom(3);
    pub const MUL: StaticAtom = StaticAtom(4);
    pub const DIVISION_BY_ZERO: StaticAtom = StaticAtom(5);
    pub const DIV: StaticAtom = StaticAtom(6);

    pub const static_atom_strings: [AtomInit<'static>;7] = [
        AtomInit::Lowercase("OK"),
        AtomInit::Lowercase("ERROR"),
        AtomInit::Lowercase("ADD"),
        AtomInit::Lowercase("SUB"),
        AtomInit::Lowercase("MUL"),
        AtomInit::Lowercase("DIVISION_BY_ZERO"),
        AtomInit::Lowercase("DIV"),
    ];
}

use atom::*;

// fn dynamic_atom(env: &mut Env, _args:&[ScopedTerm]) -> nif::Result<ScopedTerm> {
//     Ok(Atom::new(env, "an_atom").to_term(env))
// }

// fn prebuilt_atom(env: &mut Env, _args:&[ScopedTerm]) -> nif::Result<ScopedTerm> {
//     Ok(get_priv_data(env).prebuilt_atom.to_term(env))
// }



// fn term_scope<'a>(env: &'a Env, args: &[ScopedTerm]) -> nif::Result<ScopedTerm<'a>> {
//     // ScopedTerm scope test.  Try to store term in static data.  Must not compile.
//     static mut STATIC_TERM : Option<ScopedTerm<'static>> = None;
//     unsafe { STATIC_TERM = Some(args[0]); }
//     Ok(12345.bind(env).into())
// }






fn int<'a>(env: &'a Env, _args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    Ok(12345.bind(env).into())
}

fn add_ints1<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let a:i32 = try!(args[0].bind(env).try_into());
    let b:i32 = try!(args[1].bind(env).try_into());
    Ok((a+b).bind(env).into())
}


fn tuple1<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    //Ok(12345.bind(env).into())
    let (a, b, c, d) : (i32, u32, i64, u64) = try!(args[0].bind(env).try_into());
    Ok( (d, a, b, c).bind(env).into() )
}

fn tuple2<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    // Ok(12345.bind(env).into())
    let (a, b, c, d) : (i32, u32, i64, u64) = try!(args.bind(env).try_into());
    Ok( (b, c, d, a).bind(env).into() )
}

fn tuple0<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    // Ok(12345.bind(env).into())
    let (x,y) : ((),()) = try!(args.bind(env).try_into());
    Ok( (x,y,()).bind(env).into() )
}

fn catbin<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    // unpack args
    let (a,b): (&[u8], &[u8]) = try!(args.bind(env).try_into());

    // allocate binary
    let mut bin = Binary::new(a.len() + b.len());

    // copy source binaries into dest
    bin.as_mut()[..a.len()].clone_from_slice(a);
    bin.as_mut()[a.len()..].clone_from_slice(b);

    // convert to term
    let term = bin.bind(env).into();

    //bin.as_mut()[..a.len()].clone_from_slice(a); // musn't compile

    Ok(term)
}



fn staticatom<'p>(env: &'p Env, _args: &[ScopedTerm]) -> Result<ScopedTerm<'p>> {
    Ok( OK.bind(env).into() )
}

fn mathcommand<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let t: (StaticAtom, i32, i32) = try!(args.bind(env).try_into());
    match t {
        (ADD, a, b) => Ok( (OK, a+b)                 .bind(env).into() ),
        (SUB, a, b) => Ok( (OK, a-b)                 .bind(env).into() ),
        (MUL, a, b) => Ok( (OK, a*b)                 .bind(env).into() ),
        (DIV, _a,0) => Ok( (ERROR, DIVISION_BY_ZERO) .bind(env).into() ),
        (DIV, a, b) => Ok( (OK, a/b)                 .bind(env).into() ),
        _ => Err(Error::Badarg)
    }
}

fn makeresources<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let (a,b): (i32, i32) = try!(args.bind(env).try_into());
    let x = Resource::new(Cell::new(a));
    let y = Resource::new(Cell::new(b));
    Ok( (&x,y).bind(env).into())
}

fn incresources<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let (x,y,inc): (&Cell<i32>, Resource<Cell<i32>>, i32) = try!(args.bind(env).try_into());
    x.set(x.get()+inc);
    y.set(y.get()+inc);
    Ok( OK.bind(env).into() )
}

fn getresources<'a>(env: &'a Env, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let (x,y): (&Cell<i32>, Resource<Cell<i32>>) = try!(args.bind(env).try_into());
    Ok( (x.get(), y.get()).bind(env).into())
}

// fn doubleit(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     let (f,):(f64,) = try!(args.from_term(env));
//     Ok((f*2.0).to_term(env))
// }

// fn selfsend(env: &mut Env, _args: &[Term]) -> nif::Result<Term> {
//     let to_pid = selfpid(env);
//     let mut msg_env = OwnedEnv::new();
//     let msg = 9998.to_term(msg_env.as_mut());
//     unsafe{ send_from_process(env, &to_pid, msg_env, msg).unwrap() };
//     Ok(AsTerm::term_from(env, &0))
//     //Ok(0.to_term(env))
// }


// fn decode_int(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     //let num:i32 = try!(FromTerm::from_term(env, args[0]));
//     let num:i32 = try!(args[0].from_term(env));
//     Ok( (num * 2).to_term(env) )
// }



// fn tuple_math(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     let (a,b):(i32, i32) = try!(args[0].from_term(env));
//     Ok( (a+10, b*2).to_term(env) )
// }

// fn param2tuple(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     let (a,b):(i32, i32) = try!(args.from_term(env));
//     Ok( (a+10, b*2).to_term(env) )
// }


// fn make_resource(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     let (resnum,):(i32,) = try!(args.from_term(env));
//     match resnum {
//         0 => Ok(ResourcePtr::new(1239).to_term(env)),
//         1 => {
//             let x = ResourcePtr::new(1238);
//             Ok(123.to_term(env))
//         }
// //        1 => Ok(ResourcePtr::new(Cell::new(1238)).to_term(env)),
//         _ => Err(Error::Badarg),
//     }
// }

// fn render_resource(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     let (resnum, resterm):(i32, Term) = try!(args.from_term(env));
//     match resnum {
//         0 => {
//             let p: ResourcePtr<i32> = try!(resterm.from_term(env));
//             Ok((*p).to_term(env))
//         },
//         _ => Err(Error::Badarg),
//     }
// }

// fn mutate_resource(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     let (resnum, resterm, valterm):(i32, Term, Term) = try!(args.from_term(env));
//     Err(Error::Badarg)
// }


// fn reverse_binary(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
//     let (binterm,):(Term,) = try!(args.from_term(env));

//     let oldslice: &[u8] = try!(binterm.from_term(env));
//     let (newterm, newslice) = new_binary(env, oldslice.len());

//     let mut newiter = newslice.iter_mut();

//     for old in oldslice.iter().rev() {
//         *(newiter.next().unwrap()) = *old;
//     }
//     Ok(newterm)
// }















// really old stuff....

// static mut store_atom_storage:Option<Term<'static>> = None;
// unsafe_wrapper!(store_atom_wrapper, store_atom);
// fn store_atom<'a>(env:&'a Env, args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     let result = match term_to_atom(env, args[0]) {
//         Some(atom) => {
//             unsafe{ store_atom_storage = Some(atom); }
//             0
//         },
//         None => 1
//     };
//     Ok(result.to_term(env))
// }

// unsafe_wrapper!(fetch_atom_wrapper, fetch_atom);
// fn fetch_atom<'a>(env:&'a Env, _args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     let result = match unsafe{store_atom_storage} {
//         Some(atom) => atom,
//         None => 1.to_term(env)
//     };
//     Ok(result)
// }


// fn envptr<'a>(env:&'a Env, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
//     // plainintmuttest();
//     // holderintmuttest();
//     //threadtest(env);
//     //let abc = test::new_i32();
//     // let penv0 = PIE::new().map(|pie:&Env,_| {
//     //     // let it  = 0.to_term(pie);
//     //     // let it2 = 1.to_term(pie);
//     //     // println!("done it assignment, inside closure");
//     //     // it
//     //     0.to_term(pie)
//     // });

//     //let penv0 = PIE::new().map(envtest);

//     println!("done it assignment, ouside closure");


// // //     let env2 = &mut penv.as_mut();

// // // //    let it = 0.to_term(penv.as_mut());
// //     mutenvborrow(&mut env2);
// //     mutenvborrow(&mut env2);
// //     mutenvborrow(&mut env2);
// //     let it  = 0.to_term(&mut env2);
// //     let it2 = 1.to_term(&mut env2);
//     Ok(0.to_term(&env))
// }

// fn envtest<'a>(pie:&'a Env, _:()) -> Term<'a> {
//     0.to_term(pie)
// }


// fn send123<'a>(env:&'a Env, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
//     let pid = unsafe{ nif::selfpid(env) };

//     std::thread::spawn(move || {
//         unsafe {
//             //let spare = UniqueEnv::new();
//             let e1 = UniqueEnv::new();

//             let t1 = 1.to_term(e1.as_static());
//             let d1 = nif::send_from_thread(&pid, e1, t1).unwrap();
//             let e2 = d1.clear();

//             let t2 = 2.to_term(e2.as_static());
//             let e3 = nif::send_from_thread(&pid, e2, t2).unwrap().clear();

//             let t3 = 3.to_term(e3.as_static());
//             nif::send_from_thread(&pid, e3, t3);
//         }
//     });

//     Ok(0.to_term(&env))
// }



// fn mutenvborrow(blah:&mut Env) {
//     // do dee do
// }

// fn incint(i:&i32) {
//     //*i += 1;
//     println!("incint sees {}",&i);
// }

// fn plainintmuttest() {
//     let x:i32 = 123;

//     incint(&x);
//     incint(&x);
//     incint(&x);
//     println!("{}",x);

// }

// struct intholder {
//     i:i32
// }

// impl std::ops::Deref for intholder {
//     type Target = i32;
//     fn deref(&self) -> &Self::Target {
//         &self.i
//     }
// }


// fn holderintmuttest() {
//     let holder = intholder{i:456};

//     incint(&holder);
//     incint(&holder);
//     incint(&holder);
//     println!("all done");
// }

// must not compile ///////////////////////////////////////////////

// static mut my_term:Option<Term<'static>> = None;
// fn should_not_compile<'a>(env:&'a Env, args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     my_term = Some(args[0]);
//     Ok(123.to_term(env))
// }


// static mut my_term:Option<Term<'static>> = None;
// fn test_compile<'a>(env:&'a Env, args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     Ok(unsafe{my_term.unwrap()})
// }


// // must not compile ///////////////////////////////////////////////
// fn envptrxxx<'a>(env:&'a Env, _args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     let penv = EnvPtr::new();
//     let it = 0.to_term(&penv);
//     Ok(it)
// }


// look at this again when thread::scoped is back up.
// // must not compile ///////////////////////////////////////////////
// fn threadtest(env:& Env) {
//     let penv = EnvPtr::new();

//     println!("hello from before thread");

//     let threads = (0..10).map(|x| {
//         std::thread::spawn( move || {
//             let temp = 0.to_term(&penv);
//             println!("hello from thread {}",x);
//         })
//     });

//     for t in threads { t.join(); }


//     // std::thread::spawn( move || {
//     //     let temp = 0.to_term(&penv);
//     //     println!("hello from thread");
//     // }).join();

//     println!("hello from after thread");

//     let it = 0.to_term(env);

// }


// must not compile ///////////////////////////////////////////////
// must not compile ///////////////////////////////////////////////
// must not compile ///////////////////////////////////////////////
// must not compile ///////////////////////////////////////////////
// must not compile ///////////////////////////////////////////////


