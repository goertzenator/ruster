// Arbitrary Tuples

use super::*;

// convert &[Term] to Term

impl<'a, T> Bind for &'a [T]
    where T: Into<CTerm>
{}

// conert &[Term] to Term
impl<'a, 'e, T> From<Binder<'e, &'a [T]>> for ScopedTerm<'e>
    where T: Into<CTerm>
{
    fn from(b: Binder<&'a [T]>) -> Self {  // 'e elided on input and output
        ScopedTerm::new(
            unsafe {
                ens::enif_make_tuple_from_array(
                    std::mem::transmute(b.env),
                    std::mem::transmute(b.val.as_ptr()),
                    b.val.len() as ens::c_uint)
            }
        )
    }
}


// convert Term to &[Term]
impl<'a, 'e> TryFrom<Binder<'e, ScopedTerm<'a>>> for &'e [ScopedTerm<'e>] {
    type Err = Error;
    fn try_from(b: Binder<'e,ScopedTerm<'a>>) -> Result<&'e [ScopedTerm<'e>]> { // elision unclear in this case, specify everything
        unsafe {
            let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
            let mut arity: ens::c_int = std::mem::uninitialized();
            let env = b.env;
            let term = b.val;
            match ens::enif_get_tuple(env.as_api_ptr(), term.into(), &mut arity, &mut ptr) {
                0 => Err(Error::Badarg),
                _ => Ok( std::slice::from_raw_parts( ptr as *const ScopedTerm, arity as usize) ),
            }
        }
    }
}


// pub trait TestTrait3<T> {
//     fn testfrom3(t: T) -> Self;
// }

// impl<'e, T0> TestTrait3<Binder<'e, (T0,)>> for ScopedTerm<'e>
//     where Binder<'e, T0>: Into<ScopedTerm<'e>>,
//           T0: Bind
// {
//     fn testfrom3(b: Binder<'e, (T0,)>) -> ScopedTerm<'e> {
//         let tup = b.val;
//         let env = b.env;
//         let terms: [Term; 1] = [tup.0.bind(env).into()];  // StaticAtom -> Term<'static>
//         Term::new(
//             unsafe {
//                 ens::enif_make_tuple_from_array(
//                     env.as_api_ptr(),
//                     terms.as_ptr() as *const ens::ERL_NIF_TERM,
//                     terms.len() as ens::c_uint)
//             }
//         )
//         // terms.as_ref().bind(env).into()
//     }
// }



macro_rules! impl_tuple_conversion {
    ($arity:expr, [$($indices:tt),*], [$($typevars:ident),*])  => (


        //impl<'a, $($typevars),*> Bind for &'a ($($typevars),*,) {}
        impl<$($typevars),*> Bind for ($($typevars),*,)
            where
            $($typevars : Bind),*
         {}

        // convert tuple to [ScopedTerm;N]
        impl<'a, 'e, $($typevars),*> From<Binder<'e, ($($typevars),*,)>> for [ScopedTerm<'e>;$arity]
            where
            $(Binder<'e, $typevars> : Into<ScopedTerm<'e>>),*,
            $($typevars : Bind),*

        {
            fn from(b: Binder<'e, ($($typevars),*,)>) -> [ScopedTerm<'e>;$arity] {
                let tup = b.val;
                let env = b.env;
                [$(tup.$indices.bind(env).into()),*]
            }
        }

        // convert tuple to [ScopedTerm;N] to ScopedTerm
        impl<'e, $($typevars),*> From<Binder<'e, ($($typevars),*,)>> for ScopedTerm<'e>
            where
            $(Binder<'e, $typevars> : Into<ScopedTerm<'e>>),*,
            $($typevars : Bind),*
        {
            fn from(b: Binder<'e, ($($typevars),*,)>) -> ScopedTerm<'e> {
                let tup = b.val;
                let env = b.env;
                let terms:[ScopedTerm; $arity] = [$(tup.$indices.bind(env).into()),*];
//                let terms:[Term; $arity] = tup.bind(env).into();
                terms.as_ref().bind(env).into()
            }
        }

        // convert &[ScopedTerm] to tuple
        impl<'a, 'e, $($typevars),*> TryFrom<Binder<'e, &'a [ScopedTerm<'a>]>> for ($($typevars),*,)
            where
            $($typevars: TryFrom<Binder<'e, ScopedTerm<'a>>, Err = Error>),*
        {
            type Err = Error;
            fn try_from(b: Binder<'e, &'a [ScopedTerm<'a>]>) -> Result<Self>
            {
                let terms = b.val;
                let env = b.env;
                if terms.len() != $arity { return Err(Error::Badarg) };
                Ok((
                    $(try!(terms[$indices].bind(env).try_into())),*,
                ))
            }
        }

        // convert ScopedTerm to &[ScopedTerm] to tuple
        impl<'a, 'e, $($typevars),*> TryFrom<Binder<'e, ScopedTerm<'a>>> for ($($typevars),*,)
            where
            $($typevars: TryFrom<Binder<'e, ScopedTerm<'a>>, Err = Error>),*
        {
            type Err = Error;
            fn try_from(b: Binder<'e, ScopedTerm<'a>>) -> Result<Self> {
                let term = b.val;
                let env = b.env;

                //let terms:&[ScopedTerm] = try!(term.bind(env).try_into());
                // manual expansion to avoid lifetime conflicts that I don't understand
                let terms:&[ScopedTerm] = try!(unsafe {
                    let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
                    let mut arity: ens::c_int = std::mem::uninitialized();
                    match ens::enif_get_tuple(std::mem::transmute(env), term.into(), &mut arity, &mut ptr) {
                        0 => Err(Error::Badarg),
                        _ => Ok( std::slice::from_raw_parts( std::mem::transmute(ptr), arity as usize) ),
                    }
                });

                //terms.bind(env).try_into()
                // manual expansion to avoid lifetime conflicts that I don't understand
                if terms.len() != $arity { return Err(Error::Badarg) };
                Ok((
                    $(try!(terms[$indices].bind(env).try_into())),*,
                ))

            }
        }
    );
}

// impl_tuple_conversion!(0,
//  [],
//  []);

//

impl Bind for () {}

impl<'e> From<Binder<'e, ()>> for [ScopedTerm<'e>; 0]
{
    fn from(_b: Binder<'e, ()>) -> Self {
        []
    }
}

impl<'e> From<Binder<'e, ()>> for ScopedTerm<'e>
{
    fn from(b: Binder<'e, ()>) -> Self {
        let env = b.env;
        let terms: [ScopedTerm; 0] = [];
        terms.as_ref().bind(env).into()
    }
}

impl<'a, 'e> TryFrom<Binder<'e, &'a [ScopedTerm<'a>]>> for ()
{
    type Err = Error;
    fn try_from(b: Binder<'e, &'a [ScopedTerm<'a>]>) -> Result<Self> {
        let terms = b.val;
        if terms.len() != 0 {
            return Err(Error::Badarg);
        };
        Ok(())
    }
}

impl<'a, 'e> TryFrom<Binder<'e, ScopedTerm<'a>>> for ()
{
    type Err = Error;
    fn try_from(b: Binder<'e, ScopedTerm<'a>>) -> Result<Self> {
        let term = b.val;
        let env = b.env;
        let terms: &[ScopedTerm] = try!(unsafe {
            let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
            let mut arity: ens::c_int = std::mem::uninitialized();
            match ens::enif_get_tuple(std::mem::transmute(env),
                                      term.into(),
                                      &mut arity,
                                      &mut ptr) {
                0 => Err(Error::Badarg),
                _ => Ok(std::slice::from_raw_parts(std::mem::transmute(ptr), arity as usize)),
            }
        });
        if terms.len() != 0 {
            return Err(Error::Badarg);
        };
        Ok(())
    }
}



impl_tuple_conversion!(1,
    [0],
    [T0]);

impl_tuple_conversion!(2,
    [0,1],
    [T0,T1]);

impl_tuple_conversion!(3,
    [0,1,2],
    [T0,T1,T2]);

impl_tuple_conversion!(4,
    [0,1,2,3],
    [T0,T1,T2,T3]);

impl_tuple_conversion!(5,
    [0,1,2,3,4],
    [T0,T1,T2,T3,T4]);

impl_tuple_conversion!(6,
    [0,1,2,3,4,5],
    [T0,T1,T2,T3,T4,T5]);

impl_tuple_conversion!(7,
    [0,1,2,3,4,5,6],
    [T0,T1,T2,T3,T4,T5,T6]);

impl_tuple_conversion!(8,
    [0,1,2,3,4,5,6,7],
    [T0,T1,T2,T3,T4,T5,T6,T7]);

impl_tuple_conversion!(9,
    [0,1,2,3,4,5,6,7,8],
    [T0,T1,T2,T3,T4,T5,T6,T7,T8]);

impl_tuple_conversion!(10,
    [0,1,2,3,4,5,6,7,8,9],
    [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9]);


