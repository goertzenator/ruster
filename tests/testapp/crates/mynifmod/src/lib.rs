//#![feature(trace_macros)]

// #[macro_use]
// extern crate erlang_nif_sys;


#[macro_use]
extern crate ruster;

use ruster::{ProcEnv, ScopedTerm, StaticTerm, Binary, Resource, StaticAtom, Binder, Error, Bind,
             TryInto, Result, PrivData};

// use nif::{open_resource_type, new_binary, ResourcePtr, Error};
// use nif::{selfpid, send_from_process};
//use nif::erlang_nif_sys::{enif_priv_data, c_void, c_int};

use std::cell::Cell;

//trace_macros!(true);
/// Create NIF module data and init function.
ruster_init!("mynifmod",
             [("int", 0, ruster_fn!(int)),
              ("add_ints1", 2, ruster_fn!(add_ints1)),
              ("tuple1", 1, ruster_fn!(tuple1)),
              ("tuple2", 4, ruster_fn!(tuple2)),
              ("tuple0", 2, ruster_fn!(tuple0)),
              ("catbin", 2, ruster_fn!(catbin)),
              ("staticatom", 0, ruster_fn!(staticatom)),
              ("mathcommand", 3, ruster_fn!(mathcommand)),
              ("makeresources", 2, ruster_fn!(makeresources)),
              ("incresources", 3, ruster_fn!(incresources)),
              ("getresources", 2, ruster_fn!(getresources))],
             [OK, ERROR, ADD, SUB, MUL, DIVISION_BY_ZERO, DIV],
             MyType);


struct MyType;
impl PrivData for MyType {
    fn load<'e>(_env: &'e ProcEnv, _load_info: ScopedTerm<'e>) -> Self {
        MyType
    }
}

use atom::*;


// fn term_scope<'e>(env: &'e ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'e>> {
//     // ScopedTerm scope test.  Try to store term in static data.  Must not compile.
//     static mut STATIC_TERM : Option<ScopedTerm<'static>> = None;
//     unsafe { STATIC_TERM = Some(args[0]); }
//     Ok(12345.bind(env).into())
// }


fn int<'a>(env: &'a ProcEnv, _args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    Ok(12345.bind(env).into())
}

fn add_ints1<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let a: i32 = args[0].bind(env).try_into()?;
    let b: i32 = args[1].bind(env).try_into()?;
    Ok((a + b).bind(env).into())
}


fn tuple1<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    //Ok(12345.bind(env).into())
    let (a, b, c, d): (i32, u32, i64, u64) = args[0].bind(env).try_into()?;
    Ok((d, a, b, c).bind(env).into())
}

fn tuple2<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    // Ok(12345.bind(env).into())
    let (a, b, c, d): (i32, u32, i64, u64) = args.bind(env).try_into()?;
    Ok((b, c, d, a).bind(env).into())
}

fn tuple0<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    // Ok(12345.bind(env).into())
    let (x, y): ((), ()) = args.bind(env).try_into()?;
    Ok((x, y, ()).bind(env).into())
}

fn catbin<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    // unpack args
    let (a, b): (&[u8], &[u8]) = args.bind(env).try_into()?;

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



fn staticatom<'p>(env: &'p ProcEnv, _args: &[ScopedTerm]) -> Result<ScopedTerm<'p>> {
    Ok(OK.bind(env).into())
}

fn mathcommand<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let t: (StaticAtom, i32, i32) = args.bind(env).try_into()?;
    match t {
        (ADD, a, b) => Ok((OK, a + b).bind(env).into()),
        (SUB, a, b) => Ok((OK, a - b).bind(env).into()),
        (MUL, a, b) => Ok((OK, a * b).bind(env).into()),
        (DIV, _a, 0) => Ok((ERROR, DIVISION_BY_ZERO).bind(env).into()),
        (DIV, a, b) => Ok((OK, a / b).bind(env).into()),
        _ => Err(Error::Badarg),
    }
}

fn makeresources<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let (a, b): (i32, i32) = args.bind(env).try_into()?;
    let x = Resource::new(Cell::new(a));
    let y = Resource::new(Cell::new(b));
    Ok((&x, y).bind(env).into())
}

fn incresources<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let (x, y, inc): (&Cell<i32>, Resource<Cell<i32>>,  i32) = args.bind(env).try_into()?;
    x.set(x.get() + inc);
    y.set(y.get() + inc);
    Ok(OK.bind(env).into())
}

fn getresources<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let (x, y): (&Cell<i32>, Resource<Cell<i32>>) = args.bind(env).try_into()?;
    Ok((x.get(), y.get()).bind(env).into())
}



fn messages<'a>(env: &'a ProcEnv, args: &[ScopedTerm]) -> Result<ScopedTerm<'a>> {
    let (pid,) = args.bind(env).try_into()?;
    let sender = Sender::from_procenv(env);

    sender.send(pid, 1);
    sender.send(Pid::from_procenv(env), 2);
    sender.send(pid, Sender::from_thread.is_none());

    let mt = MobileTerm((OK, pid));

    move || {
        unsafe {
            mt.run(|env, term| {
                let sender = Sender::from_thread();
                let (_, pid): (ScopedTerm, Pid) = term.bind(env).try_into()?;
                sender.send(pid, 3);
                sender.send(Pid::from_procenv(env), 4);
                sender.send(pid, Sender::from_procenv(env));

            })
        }
    }
}



// fn selfsend(env: &mut ProcEnv, _args: &[Term]) -> nif::Result<Term> {
//     let to_pid = selfpid(env);
//     let mut msg_env = OwnedProcEnv::new();
//     let msg = 9998.to_term(msg_env.as_mut());
//     unsafe{ send_from_process(env, &to_pid, msg_env, msg).unwrap() };
//     Ok(AsTerm::term_from(env, &0))
//     //Ok(0.to_term(env))
// }


// fn decode_int(env: &mut ProcEnv, args: &[Term]) -> nif::Result<Term> {
//     //let num:i32 = FromTerm::from_term(env, args[0]));
//     let num:i32 = args[0].from_term(env));
//     Ok( (num * 2).to_term(env) )
// }



// fn tuple_math(env: &mut ProcEnv, args: &[Term]) -> nif::Result<Term> {
//     let (a,b):(i32, i32) = args[0].from_term(env));
//     Ok( (a+10, b*2).to_term(env) )
// }

// fn param2tuple(env: &mut ProcEnv, args: &[Term]) -> nif::Result<Term> {
//     let (a,b):(i32, i32) = args.from_term(env));
//     Ok( (a+10, b*2).to_term(env) )
// }


// fn make_resource(env: &mut ProcEnv, args: &[Term]) -> nif::Result<Term> {
//     let (resnum,):(i32,) = args.from_term(env));
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

// fn render_resource(env: &mut ProcEnv, args: &[Term]) -> nif::Result<Term> {
//     let (resnum, resterm):(i32, Term) = args.from_term(env));
//     match resnum {
//         0 => {
//             let p: ResourcePtr<i32> = resterm.from_term(env));
//             Ok((*p).to_term(env))
//         },
//         _ => Err(Error::Badarg),
//     }
// }

// fn mutate_resource(env: &mut ProcEnv, args: &[Term]) -> nif::Result<Term> {
//     let (resnum, resterm, valterm):(i32, Term, Term) = args.from_term(env));
//     Err(Error::Badarg)
// }


// fn reverse_binary(env: &mut ProcEnv, args: &[Term]) -> nif::Result<Term> {
//     let (binterm,):(Term,) = args.from_term(env));

//     let oldslice: &[u8] = binterm.from_term(env));
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
// fn store_atom<'a>(env:&'a ProcEnv, args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
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
// fn fetch_atom<'a>(env:&'a ProcEnv, _args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     let result = match unsafe{store_atom_storage} {
//         Some(atom) => atom,
//         None => 1.to_term(env)
//     };
//     Ok(result)
// }


// fn envptr<'a>(env:&'a ProcEnv, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
//     // plainintmuttest();
//     // holderintmuttest();
//     //threadtest(env);
//     //let abc = test::new_i32();
//     // let penv0 = PIE::new().map(|pie:&ProcEnv,_| {
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

// fn envtest<'a>(pie:&'a ProcEnv, _:()) -> Term<'a> {
//     0.to_term(pie)
// }


// fn send123<'a>(env:&'a ProcEnv, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
//     let pid = unsafe{ nif::selfpid(env) };

//     std::thread::spawn(move || {
//         unsafe {
//             //let spare = UniqueProcEnv::new();
//             let e1 = UniqueProcEnv::new();

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



// fn mutenvborrow(blah:&mut ProcEnv) {
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
// fn should_not_compile<'a>(env:&'a ProcEnv, args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     my_term = Some(args[0]);
//     Ok(123.to_term(env))
// }


// static mut my_term:Option<Term<'static>> = None;
// fn test_compile<'a>(env:&'a ProcEnv, args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     Ok(unsafe{my_term.unwrap()})
// }


// // must not compile ///////////////////////////////////////////////
// fn envptrxxx<'a>(env:&'a ProcEnv, _args:&[Term<'a>]) -> Result<Term<'a>,NifError> {
//     let penv = ProcEnvPtr::new();
//     let it = 0.to_term(&penv);
//     Ok(it)
// }


// look at this again when thread::scoped is back up.
// // must not compile ///////////////////////////////////////////////
// fn threadtest(env:& ProcEnv) {
//     let penv = ProcEnvPtr::new();

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
