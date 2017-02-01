// Arbitrary Tuples

use super::*;


pub trait FromTermSlice
	where Self: Sized {
	fn from_termslice<'a>(env: &'a Env, &[Term<'a>]) -> Result<Self>;
}




// pub trait ToTerm {
// 	fn to_term<'a>(&self, env: &'a mut Env) -> Term<'a>;
// }

impl<'a> ToTerm for [Term<'a>] {
	fn to_term<'b>(&self, env: &'b Env) -> Term<'b> {
		Term::new(
			unsafe {
				ens::enif_make_tuple_from_array(
					std::mem::transmute(env),
					std::mem::transmute(self.as_ptr()),
					self.len() as ens::c_uint)
			}
		)
	}
}



// pub trait FromTerm: Sized {
// //	fn from_term(env: &mut Env, term: Term) -> Result<Self>;
// //	fn from_term<'a, 'b>(env: &'a mut Env, term: Term<'b>) -> Result<Self>; // default desugar, wrong!
// 	fn from_term<'a>(env: &'a mut Env, term: Term<'a>) -> Result<Self>;
// }


// impl FromTerm for $datatype {
// 	fn from_term(env: &mut Env, term: Term) -> Result<Self> {
// 		let mut result = unsafe {std::mem::uninitialized()};
// 		match unsafe{$from(env, term.into(), &mut result)} {
// 			0 => Err(Error::Badarg),
// 			_ => Ok(result),
// 		}
// 	}
// }


impl<'a> FromTerm for &'a [Term<'a>] {
	fn from_term<'b>(env: &'b Env, term: Term<'b>) -> Result<Self> {
		unsafe {
			let mut ptr: *const ens::ERL_NIF_TERM = std::mem::uninitialized();
			let mut arity: ens::c_int = std::mem::uninitialized();
			match ens::enif_get_tuple(std::mem::transmute(env), term.into(), &mut arity, &mut ptr) {
				0 => Err(Error::Badarg),
				_ => Ok( std::slice::from_raw_parts( std::mem::transmute(ptr), arity as usize) ),
			}
		}
	}
}


macro_rules! impl_tuple_conversion {
    ($arity:expr, [$($indices:tt),*], [$($typevars:ident),*])  => (

    	// convert tuple to term
		impl<$($typevars : ToTerm),*> ToTerm for ($($typevars),*,) {
			fn to_term<'a>(&self, env: &'a Env) -> Term<'a> {
				let terms:[Term; $arity] = [$(self. $indices .to_term(env)),*];
				terms.as_ref().to_term(env)
				// unsafe {
				// 	Term::new(ens::enif_make_tuple_from_array(
				// 		std::mem::transmute(env),
				// 		std::mem::transmute(terms.as_ptr()),
				// 		terms.len() as ens::c_uint)
				// 	)
				// }
			}
		}


		// convert term to tuple
		impl<$($typevars : FromTerm),*> FromTerm for ($($typevars),*,) {
			fn from_term(env: &Env, term: Term) -> Result<Self> {
				let terms:&[Term] = try!(term.from_term(env));
				FromTermSlice::from_termslice(env, terms)
			}
		}

//    let (a, b, c, d) : (i32, u32, i64, u64) = try!(from_termslice(env, args));


		impl<$($typevars : FromTerm),*> FromTermSlice for ($($typevars),*,) {
			fn from_termslice(env: &Env, terms: &[Term]) -> Result< ($($typevars),*,) >  {
				if terms.len() != $arity { return Err(Error::Badarg) };
				Ok((
					$(try!(terms[$indices].from_term(env))),*
				))
			}
		}


		// fn from_termslice<$($typevars : FromTerm),*>(env: &mut Env, terms: &[Term]) -> Result< ($($typevars),*,) >  {
		// 	if terms.len() != $arity { return Err(Error::Badarg) };
		// 	Ok((
		// 		$(try!(terms[$indices].from_term(env))),*
		// 	))

		// }

		// // convert ?
		// impl<'a, $($typevars : FromTerm),* > FromTerm<&'a [CTerm]> for ($($typevars),*,) {
		// 	fn from_term(env: &mut Env, terms: &[CTerm]) -> Result<Self> {
		// 		if terms.len() != $arity { return Err(Error::Badarg) };
		// 		Ok((
		// 			$(try!(terms[$indices].from_term(env))),*
		// 		))
		// 	}
		// }
   	);
}

// impl_tuple_conversion!(0,
// 	[],
// 	[]);


// impl_tuple_conversion!(1,
// 	[0],
// 	[T0]);

// impl_tuple_conversion!(2,
// 	[0,1],
// 	[T0,T1]);

// impl_tuple_conversion!(3,
// 	[0,1,2],
// 	[T0,T1,T2]);

impl_tuple_conversion!(4,
	[0,1,2,3],
	[T0,T1,T2,T3]);

// impl_tuple_conversion!(5,
// 	[0,1,2,3,4],
// 	[T0,T1,T2,T3,T4]);

// impl_tuple_conversion!(6,
// 	[0,1,2,3,4,5],
// 	[T0,T1,T2,T3,T4,T5]);

// impl_tuple_conversion!(7,
// 	[0,1,2,3,4,5,6],
// 	[T0,T1,T2,T3,T4,T5,T6]);

// impl_tuple_conversion!(8,
// 	[0,1,2,3,4,5,6,7],
// 	[T0,T1,T2,T3,T4,T5,T6,T7]);

// impl_tuple_conversion!(9,
// 	[0,1,2,3,4,5,6,7,8],
// 	[T0,T1,T2,T3,T4,T5,T6,T7,T8]);

// impl_tuple_conversion!(10,
// 	[0,1,2,3,4,5,6,7,8,9],
// 	[T0,T1,T2,T3,T4,T5,T6,T7,T8,T9]);


