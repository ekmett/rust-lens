#![allow(non_camel_case_types)]
#![feature(unboxed_closures)]
#![feature(type_alias_impl_trait)]
#![feature(fn_traits)]
// #![feature(generic_associated_types)]
// #![recursion_limit = "256"]
#![allow(unused_imports)]

use std::borrow::Borrow;
use std::marker::PhantomData;

// L S -> A
pub trait Ob<S> {
  type A;
}

// L S B -> T
pub trait Settee<S,B>: Ob<S> {
  type T;
  // lens-style setters can map over function results, but since they
  // have to build a new function, and the result has to be used whenever
  // these mappings have to be pure and have a 'static lifetime
  // also, we need to actually pass the arguments by value not reference
  // unlike in the lens case
  fn map<F>(&self,s:S,f:&F) -> Self::T where
    F: Fn (<Self as Ob<S>>::A) -> B + 'static;
}

// L S A -> S
// this also make it look like the other traits here
pub trait Setter<S> : Settee<S,<Self as Ob<S>>::A> {}

impl <S,X> Setter<S> for X where X: Ob<S>, X: Settee<S,<Self as Ob<S>>::A> {}

// let's work through the most pathological setter. one that works over the results of any function
// args should be a tuple of all of the arguments
pub struct result<Args>(PhantomData<Args>);

// result can be used to map over the result of any function that is at least FnOnce
impl <Args, F : FnOnce<Args> > Ob<F> for result<Args> {
  type A = F::Output;
}

// we have to relax the lens laws a bit, because i can't package up the c++ style 'functor' i'm
// building in exactly the same heap space and configuration as the one you gave me. So let's build
// a variadic function composition operator.
//
// calls F which takes several arguments, then map G over the result, returning the final answer.
pub struct VariadicThen<Args,B,F,G>(G,F,PhantomData<(*mut Args, *mut B)>) where
  F: FnOnce<Args>, // this is at least FnOnce
  G: FnOnce(<F as FnOnce<Args>>::Output) -> B;


// f and g can be called at least once
impl <Args,B,F,G> FnOnce<Args> for VariadicThen<Args,B,F,G> where
  F: FnOnce<Args>,
  G: FnOnce(<F as FnOnce<Args>>::Output) -> B,
{
  type Output = B;
  extern "rust-call" fn call_once(self, args: Args) -> Self::Output {
    self.0(self.1.call_once(args))
  }
}

// f and g might mutate some stuff
impl <Args,B,F,G> FnMut<Args> for VariadicThen<Args,B,F,G> where
  F: FnMut<Args>,
  G: FnMut (<F as FnOnce<Args>>::Output) -> B,
{
  extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output {
    self.0(self.1.call_mut(args))
  }
}

// f and g are pure as the fallen snow
impl <Args,B,F,G> Fn<Args> for VariadicThen<Args,B,F,G> where
  F: Fn<Args>,
  G: Fn(<F as FnOnce<Args>>::Output) -> B,
{
  extern "rust-call" fn call(&self, args: Args) -> Self::Output {
    self.0(self.1.call(args))
  }
}

// now result can be used as a setter for any function. you do need to name the type args though
impl <Args, F, B: 'static> Settee<F,B> for result<Args> where
  F : FnOnce<Args> + 'static,
  <F as FnOnce<Args>>::Output : 'static, {
  type T = VariadicThen<Args,B,F,&'static dyn Fn (<F as FnOnce<Args>>::Output) -> B>;
  fn map<G>(&self,f:F,g:&G) -> Self::T where
    G: Fn (<Self as Ob<F>>::A) -> B + 'static {
    VariadicThen(g,f,PhantomData)
  }
}

type Proxy<A> = PhantomData<*mut A>;

pub trait Match<C,A> {
  type T;
  fn of(&self,sum:Result<A,C>) -> Self::T;

  fn inject(&self,_:Proxy<C>,a:A) -> Self::T { self.of(Ok(a)) }
  fn reject(&self,_:Proxy<A>,c:C) -> Self::T { self.of(Err(c)) }
}

pub trait Prism<S> : Setter<S> + Match<Self::C,Self::A,T=S> {
  type C;
  fn case(&self,s: S) -> Result<Self::A,Self::C>;

  // promote this out to partial lenses.
  fn map_prism<B,F>(&self,s:S,f:F) -> <Self as Match<Self::C,B>>::T where
    Self: Match<Self::C,B>,
    F: FnOnce (&Self::A) -> B,
  {
    self.of(self.case(s).map(|a|f(&a)))
  }
}

// type-inferrable lenses for rust can exist

// the bokeh of a lens is the stuff that is out of focus.
// this trait describes how given the lens as context, and a bokeh
// you can reassemble the result.

pub trait Bokeh<C,B> {
  type T;
  fn blur(&self,bokeh:C,b:B) -> Self::T;
}

// isomorphism lenses with constant complement, and type changing assignment
// and, unlike every other encoding I've seen for lenses in rust, usable type inference
pub trait Lens<S> where
  Self : Setter<S>,
  Self : Bokeh<Self::C,Self::A>,
  // Self : Bokeh<Self::C,Self::A,T=S>, // this precludes some instances that need to change heap reps
 {
  // type A; // what you get if you feed this lens a value of type S
  type C; // everything else that is not the particular A you are interested in

  fn focus(&self,s:S) -> (Self::C, Self::A);

  fn map_once<B,F>(&self,s:S,f:F) -> <Self as Bokeh<Self::C,B>>::T where
    Self: Bokeh<Self::C,B>,
    F: FnOnce (&Self::A) -> B
  {
    let (c, a) = self.focus(s);
    self.blur(c, f(&a))
  }

  fn get(&self,s:S) -> Self::A { self.focus(s).1 }

  fn set<B>(&self,s:S,b:B) -> <Self as Bokeh<Self::C,B>>::T where
    Self: Bokeh<Self::C,B>,
  { 
    self.blur(self.focus(s).0,b)
  }
}

pub struct fst;

impl <Y,Z> Bokeh<Z,Y> for fst {
  type T=(Y,Z);
  fn blur(&self,c:Z,b:Y) -> (Y,Z) { (b,c) }
}

impl <X,Z> Ob<(X,Z)> for fst {
  type A=X;
}

impl <X,Y,Z> Settee<(X,Z),Y> for fst {
  type T = (Y,Z);
  // lens-style setters can map over function results, but since they
  // have to build a new function, and the result has to be used whenever
  // these mappings have to be pure and have a 'static lifetime
  fn map<F>(&self,s:(X,Z),f:&F) -> Self::T where
    F: Fn (X) -> Y + 'static {
    (f(s.0),s.1)
  }
}


impl <X,Z> Lens<(X,Z)> for fst {
  type C=Z;
  fn focus(&self,s:(X,Z)) -> (Z,X) { (s.1,s.0) }
}

pub struct snd;

impl <Z,Y> Bokeh<Z,Y> for snd {
  type T = (Z,Y);
  fn blur(&self,c:Z,b:Y) -> (Z,Y) { (c,b) }
}

impl <Z,X> Ob<(Z,X)> for snd {
  type A=X;
}

impl <Z,X,Y> Settee<(Z,X),Y> for snd {
  type T = (Z,Y);
  fn map<F>(&self,s:(Z,X),f:&F) -> Self::T where
    F: Fn (X) -> Y + 'static {
    (s.0,f(s.1))
  }
}

impl <Z,X> Lens<(Z,X)> for snd {
  type C=Z;
  fn focus(&self,p:(Z,X)) -> (Z,X) { p }
}

pub struct comp<P,Q>(P,Q);

//impl <S,P,Q: 

impl <C,D,B,P,Q> Bokeh<(C,D),B> for comp<P,Q> where
  P : Bokeh<C,B>,
  Q : Bokeh<D,<P as Bokeh<C,B>>::T>,
{
  type T = <Q as Bokeh<D,<P as Bokeh<C,B>>::T>>::T;
  fn blur(&self,bokeh:(C,D),b:B) -> Self::T {
    self.1.blur(bokeh.1,self.0.blur(bokeh.0,b))
  }
}

impl <S,P,Q> Ob<S> for comp<P,Q> where
  Q : Ob<S>,
  P : Ob<<Q as Ob<S>>::A>
{
  type A = <P as Ob<<Q as Ob<S>>::A>>::A;
}

impl <S,P,Q,B> Settee<S,B> for comp<P,Q> where
  Q: Settee<S,<P as Settee<<Q as Ob<S>>::A,B>>::T>,
  P: Settee<<Q as Ob<S>>::A,B>,
{
  type T = <Q as Settee<S,<P as Settee<<Q as Ob<S>>::A,B>>::T>>::T;
  fn map<F>(&self,s:S,f:&F) -> Self::T where
    F: Fn (<Self as Ob<S>>::A) -> B + 'static {
    let comp(p,q) = self;
    q.map(s,&move |x|p.map(x,f))
  }
}

/*
impl <S,P,Q> Settee<S, <P as Ob<<Q as Ob<S>>::A>>::A> for comp<P,Q> where
  Q: Setter<S>,
  P: Setter<<Q as Ob<S>>::A>,
{
  type T = u32; // (Z,Y);
  fn map<F>(&self,s:u32,f:F) -> Self::T where
    F: Fn (&u32) -> Y + 'static {
    panic!("at the disco")
  }
}

impl <S,P,Q> Lens<S> for comp<P,Q> where
  Q: Lens<S>,
  P: Lens<<Q as Ob<S>>::A>,
{
  type C = (<P as Lens<<Q as Ob<S>>::A>>::C, <Q as Lens<S>>::C);

  fn focus(&self,s:S) -> (Self::C, Self::A) {
    let (d,a) = self.1.focus(s);
    let (c,b) = self.0.focus(a);
    ((c,d),b)
  }
}

pub trait Iso<S> : Lens<S,C=()> {}
impl <S,T: Lens<S,C=()>> Iso<S> for T {}

pub struct swap;

impl <X,Y> Bokeh<(),(X,Y)> for swap {
  type T = (Y,X);
  fn blur(&self,_:(),b:(X,Y)) -> (Y,X) { (b.1,b.0) }
}

impl <X,Y> Ob<(X,Y)> for swap {
  type A = (Y,X);
}

impl <X,Y> Settee<(X, Y), (Y, X)> for swap {
}

impl <X,Y> Lens<(X,Y)> for swap {
  type C = ();
  fn focus(&self,s:(X,Y)) -> ((),(Y,X)) { ((),(s.1,s.0)) }
}
*/

#[cfg(test)]
mod tests {

  use super::*;

  #[test]
  fn it_works() {
    assert_eq!(fst.map((1,2),&|x| x + 1),(2,2));
    assert_eq!(snd.map((1,2),|x| x + 1),(1,3));
    assert_eq!(snd.set((1,2),"hello"),(1,"hello"));
    assert_eq!(fst.get((1,2)),1);
/*
    assert_eq!(comp(fst,fst).get(((1,2),3)),1);
    assert_eq!(comp(snd,fst).get(((1,2),3)),2);
    assert_eq!(comp(snd,comp(snd,fst)).get(((1,(2,3)),4)),3);
    assert_eq!(comp(snd,comp(snd,fst)).set(((1,(2,3)),4),"hello"),((1,(2,"hello")),4));
    assert_eq!(swap.get((1,2)),(2,1));
*/
  }
}


