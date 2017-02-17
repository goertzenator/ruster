// Arbitrary Tuples

use super::*;

// convert &[Term] to Term

//convert &[Term] to Term
impl<'a, 'e, E: Env, T: Into<CTerm>> EnvFrom<&'a [T], E> for ScopedTerm<'e> {
    fn efrom(terms: &'a [T], env: &E) -> Self {
        // 'e elided on input and output
        ScopedTerm::from(unsafe {
            ens::enif_make_tuple_from_array(std::mem::transmute(env),
                                            std::mem::transmute(terms.as_ptr()),
                                            terms.len() as ens::c_uint)
        })
    }
}


// convert Term to &[Term]
impl<'a, 'e, E: Env> TryEnvFrom<ScopedTerm<'a>, E> for &'e [ScopedTerm<'e>] {
    type Err = Error;
    fn try_efrom(term: ScopedTerm<'a>, env: &E) -> Result<&'e [ScopedTerm<'e>]> {
        // elision unclear in this case, specify everything
        unsafe {
            let mut ptr: *const CTerm = std::mem::uninitialized();
            let mut arity: ens::c_int = std::mem::uninitialized();
            match ens::enif_get_tuple(env.into_ptr(), term.into(), &mut arity, &mut ptr) {
                0 => Err(Error::Badarg),
                _ => Ok(std::slice::from_raw_parts(ptr as *const ScopedTerm, arity as usize)),
            }
        }
    }
}


macro_rules! impl_tuple_conversion {
    ($arity:expr, [$($indices:tt),*], [$($typevars:ident),*])  => (

        // // convert tuple to [ScopedTerm;N]
        // impl<'a, 'e, E:Env, $($typevars),*>
        //     EnvFrom<($($typevars),*,), E> for [ScopedTerm<'e>;$arity]
        //     where
        //     $($typevars : EnvInto<ScopedTerm<'e>, E>),*
        // {
        //     fn efrom(tup: ($($typevars),*,), env: &E) -> [ScopedTerm<'e>;$arity] {
        //         [$(tup.$indices.einto(env)),*]
        //     }
        // }

        // convert tuple to [ScopedTerm;N] to ScopedTerm
        impl<'e, E:Env, $($typevars),*>
            EnvFrom<($($typevars),*,), E> for ScopedTerm<'e>
            where
            $($typevars : EnvInto<ScopedTerm<'e>, E>),*
        {
            fn efrom(tup: ($($typevars),*,), env: &E) -> ScopedTerm<'e> {
                let terms:[ScopedTerm; $arity] = [$(tup.$indices.einto(env)),*];
//                let terms:[ScopedTerm; $arity] = tup.einto(env);
                terms.as_ref().einto(env)
            }
        }

        // convert &[ScopedTerm] to tuple
        impl<'a, 'e, E:Env, $($typevars),*>
            TryEnvFrom<&'a [ScopedTerm<'a>], E> for ($($typevars),*,)
            where
            $($typevars: TryEnvFrom<ScopedTerm<'a>, E, Err = Error>),*
            //$($typevars: TryEnvFrom<ScopedTerm<'a>, E>),*
        {
            type Err = Error;
            fn try_efrom(terms: &'a [ScopedTerm<'a>], env: &E) -> Result<Self>
            {
                if terms.len() != $arity { return Err(Error::Badarg) };
                Ok((
                    $(terms[$indices].try_einto(env)?),*,
                ))
            }
        }

        // convert ScopedTerm to &[ScopedTerm] to tuple
        impl<'a, 'e, E:Env, $($typevars),*> TryEnvFrom<ScopedTerm<'a>, E> for ($($typevars),*,)
            where
            $($typevars: TryEnvFrom<ScopedTerm<'a>, E, Err = Error>),*
        {
            type Err = Error;
            fn try_efrom(term: ScopedTerm<'a>, env: &E) -> Result<Self> {
                //let terms:&[ScopedTerm] = try!(term.bind(env).try_into());
                // manual expansion to avoid lifetime conflicts that I don't understand
                let terms:&[ScopedTerm] = try!(unsafe {
                    let mut ptr: *const CTerm = std::mem::uninitialized();
                    let mut arity: ens::c_int = std::mem::uninitialized();
                    match ens::enif_get_tuple(env.into_ptr(), term.into(), &mut arity, &mut ptr) {
                        0 => Err(Error::Badarg),
                        _ => Ok( std::slice::from_raw_parts( ptr as *const ScopedTerm, arity as usize) ),
                    }
                });

                //terms.bind(env).try_into()
                // manual expansion to avoid lifetime conflicts that I don't understand
                if terms.len() != $arity { return Err(Error::Badarg) };
                Ok((
                    $(try!(terms[$indices].try_einto(env))),*,
                ))

            }
        }
    );
}

// impl_tuple_conversion!(0,
//  [],
//  []);

//

// already impl by From catchall?
// impl<'e, E:Env> EnvFrom<(), E> for [ScopedTerm<'e>; 0] {
//     fn efrom(_tup: (), _env: &E) -> Self {
//         []
//     }
// }

impl<'e, E:Env> EnvFrom<(), E> for ScopedTerm<'e> {
    fn efrom(_tup: (), env: &E) -> Self {
        let terms: [ScopedTerm; 0] = [];
        terms.as_ref().einto(env)
    }
}

impl<'a, 'e, E: Env> TryEnvFrom<&'a [ScopedTerm<'a>], E> for () {
    type Err = Error;
    fn try_efrom(terms: &'a [ScopedTerm<'a>], _env: &E) -> Result<Self> {
        if terms.len() != 0 {
            return Err(Error::Badarg);
        };
        Ok(())
    }
}

impl<'a, 'e, E: Env> TryEnvFrom<ScopedTerm<'a>, E> for () {
    type Err = Error;
    fn try_efrom(term: ScopedTerm<'a>, env: &E) -> Result<Self> {
        let terms: &[ScopedTerm] = try!(unsafe {
            let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
            let mut arity: ens::c_int = std::mem::uninitialized();
            match ens::enif_get_tuple(env.into_ptr(), term.into(), &mut arity, &mut ptr) {
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



impl_tuple_conversion!(1, [0], [T0]);

impl_tuple_conversion!(2, [0, 1], [T0, T1]);

impl_tuple_conversion!(3, [0, 1, 2], [T0, T1, T2]);

impl_tuple_conversion!(4, [0, 1, 2, 3], [T0, T1, T2, T3]);

impl_tuple_conversion!(5, [0, 1, 2, 3, 4], [T0, T1, T2, T3, T4]);

impl_tuple_conversion!(6, [0, 1, 2, 3, 4, 5], [T0, T1, T2, T3, T4, T5]);

impl_tuple_conversion!(7, [0, 1, 2, 3, 4, 5, 6], [T0, T1, T2, T3, T4, T5, T6]);

impl_tuple_conversion!(8,
                       [0, 1, 2, 3, 4, 5, 6, 7],
                       [T0, T1, T2, T3, T4, T5, T6, T7]);

impl_tuple_conversion!(9,
                       [0, 1, 2, 3, 4, 5, 6, 7, 8],
                       [T0, T1, T2, T3, T4, T5, T6, T7, T8]);

impl_tuple_conversion!(10,
                       [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                       [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]);
