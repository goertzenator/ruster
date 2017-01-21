

#[macro_use]
extern crate erlang_nif_sys;

#[macro_use]
extern crate ruster as nif;

use nif::{Env, Term, CTerm, AsTerm, TermAs, OwnedEnv, Atom};
use nif::{open_resource_type, new_binary, ResourcePtr, Error};
use nif::{selfpid, send_from_process};
use erlang_nif_sys::{enif_priv_data, c_void, c_int};

use std::cell::Cell;

/// Create NIF module data and init function.
nif_init!(b"test_nif\0", Some(load), None, None, Some(unload),
  nif!(b"dynamic_atom\0", 0, dynamic_atom_wrapper),
  nif!(b"prebuilt_atom\0", 0, prebuilt_atom_wrapper),
  nif!(b"int\0", 0, int_wrapper),
  nif!(b"doubleit\0", 1, doubleit_wrapper),
  nif!(b"selfsend\0", 0, selfsend_wrapper),
  nif!(b"decode_int\0", 1, decode_int_wrapper),
  nif!(b"tuple_math\0", 1, tuple_math_wrapper),
  nif!(b"param2tuple\0", 2, param2tuple_wrapper),
  nif!(b"make_resource\0", 1, make_resource_wrapper),
  nif!(b"render_resource\0", 2, render_resource_wrapper),
  nif!(b"mutate_resource\0", 3, mutate_resource_wrapper),
  nif!(b"reverse_binary\0", 1, reverse_binary_wrapper)
);






/////////////////////////////////
// NIF Private Data


struct PrivData {
    prebuilt_atom: Atom,
}

impl PrivData {
    fn new(env: &mut Env) -> PrivData {
        PrivData {
            prebuilt_atom: Atom::new(env, "prebuilt_atom"),
        }
    }
}

fn get_priv_data(env: &mut Env) -> &'static PrivData {
    unsafe {
        &*(enif_priv_data(env) as *mut PrivData)
    }
}

// #[derive(Debug)]
// struct Droppable;

// impl Drop for Droppable {
//     fn drop(&mut self) {
//         println!("dropping dropable!");
//     }
// }


extern "C" fn load(penv: *mut Env,
                   priv_data: *mut *mut c_void,
                   _load_info: CTerm)-> c_int {

    let env = unsafe{ &mut *penv };
    let data = Box::new(PrivData::new(env));
    unsafe{ *priv_data = Box::into_raw(data) as *mut c_void; }

    unsafe {
        open_resource_type::<i32>(env, "i32").unwrap();
        open_resource_type::<Cell<i32>>(env, "Cell<i32>").unwrap();
    }
    0
}

extern "C" fn unload(_penv: *mut Env,
                     priv_data: *mut c_void) {
    unsafe {
        Box::from_raw(priv_data as *mut PrivData); // rebox and drop
    }
}


nif_wrapper!(dynamic_atom_wrapper, dynamic_atom);
nif_wrapper!(prebuilt_atom_wrapper, prebuilt_atom);
nif_wrapper!(int_wrapper, int);
nif_wrapper!(doubleit_wrapper, doubleit);
nif_wrapper!(selfsend_wrapper, selfsend);
nif_wrapper!(decode_int_wrapper, decode_int);
nif_wrapper!(tuple_math_wrapper, tuple_math);
nif_wrapper!(param2tuple_wrapper, param2tuple);
nif_wrapper!(make_resource_wrapper, make_resource);
nif_wrapper!(render_resource_wrapper, render_resource);
nif_wrapper!(mutate_resource_wrapper, mutate_resource);
nif_wrapper!(reverse_binary_wrapper, reverse_binary);

fn dynamic_atom(env: &mut Env, _args:&[Term]) -> nif::Result<Term> {
    Ok(Atom::new(env, "an_atom").as_term(env))
}

fn prebuilt_atom(env: &mut Env, _args:&[Term]) -> nif::Result<Term> {
    Ok(get_priv_data(env).prebuilt_atom.as_term(env))
}

fn int(env: &mut Env, _args: &[Term]) -> nif::Result<Term> {
    Ok(12345.as_term(env))
}

fn doubleit(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    let (f,):(f64,) = try!(args.term_as(env));
    Ok((f*2.0).as_term(env))
}

fn selfsend(env: &mut Env, _args: &[Term]) -> nif::Result<Term> {
    let to_pid = selfpid(env);
    let mut msg_env = OwnedEnv::new();
    let msg = 9998.as_term(msg_env.as_mut());
    unsafe{ send_from_process(env, &to_pid, msg_env, msg).unwrap() };
    Ok(AsTerm::term_from(env, &0))
    //Ok(0.as_term(env))
}


fn decode_int(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    //let num:i32 = try!(FromTerm::from_term(env, args[0]));
    let num:i32 = try!(args[0].term_as(env));
    Ok( (num * 2).as_term(env) )
}



fn tuple_math(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    let (a,b):(i32, i32) = try!(args[0].term_as(env));
    Ok( (a+10, b*2).as_term(env) )
}

fn param2tuple(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    let (a,b):(i32, i32) = try!(args.term_as(env));
    Ok( (a+10, b*2).as_term(env) )
}


fn make_resource(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    let (resnum,):(i32,) = try!(args.term_as(env));
    match resnum {
        0 => Ok(ResourcePtr::new(1239).as_term(env)),
        1 => {
            let x = ResourcePtr::new(1238);
            Ok(123.as_term(env))
        }
//        1 => Ok(ResourcePtr::new(Cell::new(1238)).as_term(env)),
        _ => Err(Error::Badarg),
    }
}

fn render_resource(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    let (resnum, resterm):(i32, Term) = try!(args.term_as(env));
    match resnum {
        0 => {
            let p: ResourcePtr<i32> = try!(resterm.term_as(env));
            Ok((*p).as_term(env))
        },
        _ => Err(Error::Badarg),
    }
}

fn mutate_resource(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    let (resnum, resterm, valterm):(i32, Term, Term) = try!(args.term_as(env));
    Err(Error::Badarg)
}


fn reverse_binary(env: &mut Env, args: &[Term]) -> nif::Result<Term> {
    let (binterm,):(Term,) = try!(args.term_as(env));

    let oldslice: &[u8] = try!(binterm.term_as(env));
    let (newterm, newslice) = new_binary(env, oldslice.len());

    let mut newiter = newslice.iter_mut();

    for old in oldslice.iter().rev() {
        *(newiter.next().unwrap()) = *old;
    }
    Ok(newterm)
}


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


