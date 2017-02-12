// Arbitrary Tuples

use super::*;

// convert &[Term] to Term

impl<'a, T> Bind for &'a [T] where T: Into<CTerm> {}

// conert &[Term] to Term
impl<'a, 'e, E: Env, T> From<Binder<'e, E, &'a [T]>> for ScopedTerm<'e>
    where T: Into<CTerm>
{
    fn from(b: Binder<E, &'a [T]>) -> Self {
        // 'e elided on input and output
        ScopedTerm::new(unsafe {
            ens::enif_make_tuple_from_array(std::mem::transmute(b.env),
                                            std::mem::transmute(b.val.as_ptr()),
                                            b.val.len() as ens::c_uint)
        })
    }
}


// convert Term to &[Term]
impl<'a, 'e, E: Env> TryFrom<Binder<'e, E, ScopedTerm<'a>>> for &'e [ScopedTerm<'e>] {
    type Err = Error;
    fn try_from(b: Binder<'e, E, ScopedTerm<'a>>) -> Result<&'e [ScopedTerm<'e>]> {
        // elision unclear in this case, specify everything
        unsafe {
            let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
            let mut arity: ens::c_int = std::mem::uninitialized();
            let env = b.env;
            let term = b.val;
            match ens::enif_get_tuple(env.as_api_ptr(), term.into(), &mut arity, &mut ptr) {
                0 => Err(Error::Badarg),
                _ => Ok(std::slice::from_raw_parts(ptr as *const ScopedTerm, arity as usize)),
            }
        }
    }
}


macro_rules! impl_tuple_conversion {
    ($arity:expr, [$($indices:tt),*], [$($typevars:ident),*])  => (


        //impl<'a, $($typevars),*> Bind for &'a ($($typevars),*,) {}
        impl<$($typevars),*> Bind for ($($typevars),*,)
            where
            $($typevars : Bind),*
         {}

        // convert tuple to [ScopedTerm;N]
        impl<'a, 'e, E:Env, $($typevars),*> From<Binder<'e, E, ($($typevars),*,)>> for [ScopedTerm<'e>;$arity]
            where
            $(Binder<'e, E, $typevars> : Into<ScopedTerm<'e>>),*,
            $($typevars : Bind),*

        {
            fn from(b: Binder<'e, E, ($($typevars),*,)>) -> [ScopedTerm<'e>;$arity] {
                let tup = b.val;
                let env = b.env;
                [$(tup.$indices.bind(env).into()),*]
            }
        }

        // convert tuple to [ScopedTerm;N] to ScopedTerm
        impl<'e, E:Env, $($typevars),*> From<Binder<'e, E, ($($typevars),*,)>> for ScopedTerm<'e>
            where
            $(Binder<'e, E, $typevars> : Into<ScopedTerm<'e>>),*,
            $($typevars : Bind),*
        {
            fn from(b: Binder<'e, E, ($($typevars),*,)>) -> ScopedTerm<'e> {
                let tup = b.val;
                let env = b.env;
                let terms:[ScopedTerm; $arity] = [$(tup.$indices.bind(env).into()),*];
//                let terms:[Term; $arity] = tup.bind(env).into();
                terms.as_ref().bind(env).into()
            }
        }

        // convert &[ScopedTerm] to tuple
        impl<'a, 'e, E:Env, $($typevars),*> TryFrom<Binder<'e, E, &'a [ScopedTerm<'a>]>> for ($($typevars),*,)
            where
            $($typevars: TryFrom<Binder<'e, E, ScopedTerm<'a>>, Err = Error>),*
        {
            type Err = Error;
            fn try_from(b: Binder<'e, E, &'a [ScopedTerm<'a>]>) -> Result<Self>
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
        impl<'a, 'e, E:Env, $($typevars),*> TryFrom<Binder<'e, E, ScopedTerm<'a>>> for ($($typevars),*,)
            where
            $($typevars: TryFrom<Binder<'e, E, ScopedTerm<'a>>, Err = Error>),*
        {
            type Err = Error;
            fn try_from(b: Binder<'e, E, ScopedTerm<'a>>) -> Result<Self> {
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

impl<'e, E: Env> From<Binder<'e, E, ()>> for [ScopedTerm<'e>; 0] {
    fn from(_b: Binder<'e, E, ()>) -> Self {
        []
    }
}

impl<'e, E: Env> From<Binder<'e, E, ()>> for ScopedTerm<'e> {
    fn from(b: Binder<'e, E, ()>) -> Self {
        let env = b.env;
        let terms: [ScopedTerm; 0] = [];
        terms.as_ref().bind(env).into()
    }
}

impl<'a, 'e, E: Env> TryFrom<Binder<'e, E, &'a [ScopedTerm<'a>]>> for () {
    type Err = Error;
    fn try_from(b: Binder<'e, E, &'a [ScopedTerm<'a>]>) -> Result<Self> {
        let terms = b.val;
        if terms.len() != 0 {
            return Err(Error::Badarg);
        };
        Ok(())
    }
}

impl<'a, 'e, E: Env> TryFrom<Binder<'e, E, ScopedTerm<'a>>> for () {
    type Err = Error;
    fn try_from(b: Binder<'e, E, ScopedTerm<'a>>) -> Result<Self> {
        let term = b.val;
        let env = b.env;
        let terms: &[ScopedTerm] = try!(unsafe {
            let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
            let mut arity: ens::c_int = std::mem::uninitialized();
            match ens::enif_get_tuple(std::mem::transmute(env), term.into(), &mut arity, &mut ptr) {
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
