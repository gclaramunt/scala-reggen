package reggen

/**
 Describes a type in terms of a polinomial functor and provides conversions to and from it
*/

/*
 type family PF a :: * -> *

  class Regular a where
     from :: a -> ((PF a) a)
     to   :: ((PF a) a) -> a

*/
trait Regular[T]{
  type PF[_]
  val ff:Functor[PF] 
  def from(t:T):PF[T]
  def to(pf:PF[T]):T
}

object GenericFold {

  /**
    Generic fold
  */
  def fold[A,D](d:D)(h:Regular[D]#PF[A]=>A)(implicit r:Regular[D]):A =
    h(r.ff.fmap(r.from(d))(fold(_)(h)))

  /**
    Generic map
  */
  /*
   NOT YET... NEEDS BIFUNCTOR
  def pmap[A,B,D](d:D)(f:A=>B)(implicit r:Regular[D]):D = 
    r.to( r.ff.fmap ( r.from(d) )(pmap(_)(f)))
    */


}
