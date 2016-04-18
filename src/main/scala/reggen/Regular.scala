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
  type A
  type PF[X] <: PatternFuctor[T,X]
  val ff:Functor[PF]
  def from(t:T):PF[T]
  def to(pf:PF[T]):T
}

object Regular {

  /**
    Generic fold


-- | Definicion generica del operador fold.
-- | Esta definicion es valida para todo tipo "d" que sea instancia de Regular
-- | y cuyo functor "PF d" sea instancia de Functor.

fold  :: (Regular d, Functor (PF d)) => (PF d a -> a) -> d -> a
fold h = h . fmap (fold h) . from

  */
  def fold[A,D](d:D)(h:Regular[D]#PF[A]=>A)(implicit r:Regular[D]):A = {
    h(r.ff.fmap(r.from(d))(fold(_)(h)))
  }

}
