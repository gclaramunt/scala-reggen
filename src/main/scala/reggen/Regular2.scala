package reggen

import scala.language.higherKinds


//parametric regular types
object Regular2Bifunctors {


trait Regular2[T[_]]{
  type PF2[_,_]
  val bf:Bifunctor[PF2]
  def from2[A](t:T[A]):PF2[A,T[A]]
  def to2[A](pf:PF2[A,T[A]]):T[A]
}
  /*
  -- | *********************************************
  -- | ** Definicion generica del functor de tipo **
  -- | *********************************************

  pmap :: (Regular2 d, Bifunctor (PF2 d)) => (a -> b) -> d a -> d b
  pmap f = to2 . bimap f (pmap f) . from2 
        == to2(bimap f (pmap f) from2 da )  
*/

  def pmap[A,B,D[_]](da:D[A])(f:A=>B)(implicit r2:Regular2[D]):D[B]={
    //bimap :: (a -> b) -> (r -> s) -> f a r -> f b s
    r2.to2(r2.bf.bimap(r2.from2(da))(f, (pmap(_)(f))))
  }

/*
  -- | *******************************************
  -- | ** Definicion generica del operador fold **
  -- | *******************************************

  fold2  :: (Regular2 d, Bifunctor (PF2 d)) => (PF2 d a b -> b) -> d a -> b
  fold2 h = h . bimap id (fold2 h) . from2
*/
  def fold2[A,B,D[_]](da:D[A])(h:Regular2[D]#PF2[A,B]=>B)(implicit r2:Regular2[D]):B=
    h(r2.bf.bimap(r2.from2(da))( identity ,fold2(_)(h)))

/*
  -- Alternativamente:
  -- pmap f = fold2 (to2 . bimap f id)
*/
}
