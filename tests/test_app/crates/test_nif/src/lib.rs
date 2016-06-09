

#[macro_use]
extern crate erlang_nif_sys;

#[macro_use]
extern crate ruster as nif;

use nif::{UniqueEnv, Env, Term, Encodable};


/// Create NIF module data and init function.
nif_init!(b"test_nif\0", None, None, None, None,
  nif!(b"atom\0", 0, atom_wrapper),
  nif!(b"int\0", 0, int_wrapper)
//  nif!(b"store_atom\0",  1, store_atom_wrapper),
//  nif!(b"fetch_atom\0",  0, fetch_atom_wrapper),
//  nif!(b"envptr\0",      0, envptr_wrapper),
//  nif!(b"send123\0",     0, send123_wrapper)
  );


struct PrivData {
    
}

fn priv(env: &Env) -> 

nif_wrapper!(atom_wrapper, atom);
nif_wrapper!(int_wrapper, int);
//nif_wrapper!(send123_wrapper, send123);

fn atom<'a>(env:&'a Env, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
    Ok(123.to_term(env))
}

fn int<'a>(env:&'a Env, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
    Ok(12345.to_term(env))
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


fn envptr<'a>(env:&'a Env, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
    // plainintmuttest();
    // holderintmuttest();
    //threadtest(env);
    //let abc = test::new_i32();
    // let penv0 = PIE::new().map(|pie:&Env,_| {
    //     // let it  = 0.to_term(pie);
    //     // let it2 = 1.to_term(pie);
    //     // println!("done it assignment, inside closure");
    //     // it
    //     0.to_term(pie)
    // });

    //let penv0 = PIE::new().map(envtest);

    println!("done it assignment, ouside closure");


// //     let env2 = &mut penv.as_mut();

// // //    let it = 0.to_term(penv.as_mut());
//     mutenvborrow(&mut env2);
//     mutenvborrow(&mut env2);
//     mutenvborrow(&mut env2);
//     let it  = 0.to_term(&mut env2);
//     let it2 = 1.to_term(&mut env2);
    Ok(0.to_term(&env))
}

fn envtest<'a>(pie:&'a Env, _:()) -> Term<'a> {
    0.to_term(pie)
}


fn send123<'a>(env:&'a Env, _args:&[Term<'a>]) -> nif::Result<Term<'a>> {
    let pid = unsafe{ nif::selfpid(env) }; 

    std::thread::spawn(move || {
        unsafe {
            //let spare = UniqueEnv::new();
            let e1 = UniqueEnv::new();

            let t1 = 1.to_term(e1.as_static());
            let d1 = nif::send_from_thread(&pid, e1, t1).unwrap();
            let e2 = d1.clear();

            let t2 = 2.to_term(e2.as_static());
            let e3 = nif::send_from_thread(&pid, e2, t2).unwrap().clear();
            
            let t3 = 3.to_term(e3.as_static());
            nif::send_from_thread(&pid, e3, t3);
        }
    });

    Ok(0.to_term(&env))
}



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


