// Arbitrary Tuples

use super::*;

// convert &[Term] to Term

impl<'a> Bind for &'a [Term<'a>] {}

impl<'a, 'b> From<Binder<'b, &'a [Term<'a>]>> for Term<'b> {
    fn from(b: Binder<'b, &'a [Term<'a>]>) -> Self {
        Term::new(
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
impl<'a, 'b> TryFrom<Binder<'b, Term<'a>>> for &'b [Term<'b>] {
    type Err = Error;
    fn try_from(b: Binder<Term>) -> Result<Self> {
        unsafe {
            let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
            let mut arity: ens::c_int = std::mem::uninitialized();
            let env = b.env;
            let term = b.val;
            match ens::enif_get_tuple(std::mem::transmute(env), term.into(), &mut arity, &mut ptr) {
                0 => Err(Error::Badarg),
                _ => Ok( std::slice::from_raw_parts( std::mem::transmute(ptr), arity as usize) ),
            }
        }
    }
}


macro_rules! impl_tuple_conversion {
    ($arity:expr, [$($indices:tt),*], [$($typevars:ident),*])  => (


        impl<'a, $($typevars),*> Bind for &'a ($($typevars),*,) {}
        impl<$($typevars),*> Bind for ($($typevars),*,) {}

        // convert tuple to [Term;N]
        impl<'a, 'b, $($typevars),*> From<Binder<'b, ($($typevars),*,)>> for [Term<'b>;$arity]
            where
            $(Binder<'b, $typevars> : Into<Term<'b>>),*,
            $($typevars : Bind),*

        {
            fn from(b: Binder<'b, ($($typevars),*,)>) -> Self {
                let tup = b.val;
                let env = b.env;
                [$(tup.$indices.bind(env).into()),*]
            }
        }

        // convert tuple to [Term;N] to Term
        impl<'a, 'b, $($typevars),*> From<Binder<'b, ($($typevars),*,)>> for Term<'b>
            where
            $(Binder<'b, $typevars> : Into<Term<'b>>),*,
            $($typevars : Bind),*
        {
            fn from(b: Binder<'b, ($($typevars),*,)>) -> Self {
                let tup = b.val;
                let env = b.env;
//              let terms:[Term; $arity] = [$(tup.$indices.bind(env).into()),*];
                let terms:[Term; $arity] = tup.bind(env).into();
                terms.as_ref().bind(env).into()
            }
        }

        // convert &[Term] to tuple
        impl<'a, 'b, $($typevars),*> TryFrom<Binder<'b, &'a [Term<'a>]>> for ($($typevars),*,)
            where
            $($typevars: TryFrom<Binder<'b, Term<'a>>, Err = Error>),*
        {
            type Err = Error;
            fn try_from(b: Binder<'b, &'a [Term<'a>]>) -> Result<Self>
            {
                let terms = b.val;
                let env = b.env;
                if terms.len() != $arity { return Err(Error::Badarg) };
                Ok((
                    $(try!(terms[$indices].bind(env).try_into())),*,
                ))
            }
        }

        // convert Term to &[Term] to tuple
        impl<'a, 'b, $($typevars),*> TryFrom<Binder<'b, Term<'a>>> for ($($typevars),*,)
            where
            $($typevars: TryFrom<Binder<'b, Term<'a>>, Err = Error>),*
        {
            type Err = Error;
            fn try_from(b: Binder<'b, Term<'a>>) -> Result<Self> {
                let term = b.val;
                let env = b.env;

                //let terms:&[Term] = try!(term.bind(env).try_into());
                // manual expansion to avoid lifetime conflicts that I don't understand
                let terms:&[Term] = try!(unsafe {
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

impl<'b> From<Binder<'b, ()>> for [Term<'b>; 0]
{
    fn from(_b: Binder<'b, ()>) -> Self {
        []
    }
}

impl<'b> From<Binder<'b, ()>> for Term<'b>
{
    fn from(b: Binder<'b, ()>) -> Self {
        let env = b.env;
        let terms: [Term; 0] = [];
        terms.as_ref().bind(env).into()
    }
}

impl<'a, 'b> TryFrom<Binder<'b, &'a [Term<'a>]>> for ()
{
    type Err = Error;
    fn try_from(b: Binder<'b, &'a [Term<'a>]>) -> Result<Self> {
        let terms = b.val;
        if terms.len() != 0 {
            return Err(Error::Badarg);
        };
        Ok(())
    }
}

impl<'a, 'b> TryFrom<Binder<'b, Term<'a>>> for ()
{
    type Err = Error;
    fn try_from(b: Binder<'b, Term<'a>>) -> Result<Self> {
        let term = b.val;
        let env = b.env;
        let terms: &[Term] = try!(unsafe {
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


