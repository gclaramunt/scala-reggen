package reggen

/**
 Describes a type in terms of a polinomial functor and provides conversions to and from it
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
  def fold[A,D](d:D)(h:Regular[D]#PF[A]=>A)(implicit r:Regular[D]):A=
    h(r.ff.fmap(r.from(d))(fold(_)(h)))

}