package reggen

import scala.language.higherKinds
import scala.language.implicitConversions

//non-parametric regular types
object Regular extends App{


  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  case class K[A,R](unK:A)

  implicit def FK[A] =new Functor[({type λ[B]=K[A,B]})#λ]{
    def fmap[A1, B1](ka: K[A,A1])(f: A1 => B1): K[A,B1]=K(ka.unK)
  }

  case class U[Z]()

  implicit def FU =new Functor[U]{
    def fmap[A, B](ua: U[A])(f: A => B):U[B]=U()
  }

  case class I[Z](unI:Z)
  implicit def FId =new Functor[I]{
    def fmap[A, B](ida: I[A])(f: A => B):I[B]=I(f(ida.unI))
  }


  trait FComb[F[_],G[_],K[_,_]]{
    type abs[A]= K[F[A],G[A]]
  }

  trait :+:[F,G] 

  case class L[F,G](f:F) extends :+:[F,G]
  case class R[F,G](g:G) extends :+:[F,G]


  implicit def fplus[F[_], G[_]](implicit ff:Functor[F], fg:Functor[G])=new Functor[FComb[F,G,:+:]#abs]{
    def fmap[A,B](fga:F[A]:+:G[A])(f: A => B):F[B]:+:G[B]= fga match {
      case L(lf) => L(ff.fmap(lf)(f))
      case R(rg) => R(fg.fmap(rg)(f))
    }
  }

  case class :*:[F,G](f:F,g:G) 

  implicit def fstar[F[_], G[_]](implicit ff:Functor[F], fg:Functor[G])=new Functor[FComb[F,G,:*:]#abs]{
    def fmap[A,B](fga:F[A]:*:G[A])(f: A => B):F[B]:*:G[B]= :*:(ff.fmap(fga.f)(f),fg.fmap(fga.g)(f))
  }

  case class Comp[F[_],G[_],Z](unComp:F[G[Z]])  

  implicit def fcomp[F[_], G[_]](implicit ff:Functor[F], fg:Functor[G])=new Functor[({ type abs[A]=Comp[F,G,A]})#abs]{
    def fmap[A,B](fga:Comp[F,G,A])(f: A => B):Comp[F,G,B]= Comp(ff.fmap(fga.unComp)(fg.fmap(_)(f)))
  }

  trait Regular[T]{
    type PF[_]
    val ff:Functor[PF] 
    def from(t:T):PF[T]
    def to(pf:PF[T]):T
  }

  trait TreeInt{}
  case class LeafI(i:Int) extends TreeInt
  case class NodeI(l:TreeInt,r:TreeInt) extends TreeInt

  implicit def regularTreeInt:Regular[TreeInt]=new Regular[TreeInt]{

    //type PF[Z] = K[Int,Z]:+:(I[Z]:*:I[Z])
    type PFL[Z] = K[Int,Z]
    type PFR[Z] = (I[Z]:*:I[Z])
    type PF[Z]=PFL[Z]:+:PFR[Z]

    val ff=implicitly[Functor[PF]]

    def from(t:TreeInt):PF[TreeInt] = t match {
      case LeafI(n) => L(K(n))
      case NodeI(x,y) => R(:*:(I(x),I(y)))
    }

    def to(pf:PF[TreeInt]):TreeInt=  pf match {
      case L(K(n)) => LeafI(n)
      case R(:*:(I(x),I(y))) => NodeI(x,y)
    }
  }


implicit def regularList[A]:Regular[List[A]]=new Regular[List[A]]{

    //type PF[Z] = U[Z]:+:(K[A,Z]:*:I[Z])

    type PFL[Z] = U[Z]
    type PFK[Z] = K[A,Z]
    type PFR[Z] = PFK[Z]:*:I[Z]
    type PF[Z] = PFL[Z]:+:PFR[Z]

    val ff=implicitly[Functor[PF]]
    
    def from(t:List[A]):PF[List[A]] = t match {
      case Nil => L(U())
      case x::xs => R(:*:(K(x),I(xs)))
    }

    def to(pf:PF[List[A]]):List[A]=  pf match {
      case L(U()) => Nil
      case R(:*:(K(x),I(xs))) => x::xs
    }
  }


  def fold[A,D](d:D)(h:Regular[D]#PF[A]=>A)(implicit r:Regular[D]):A=
    h(r.ff.fmap(r.from(d))(fold(_)(h)))


  def sum[Z]:Regular[Z]#PF[Int]=>Int = {
    case U() => 0
    case k:K[Int,Z] @unchecked=> k.unK
    case i:I[Int] @unchecked=> i.unI
    case l:L[Regular[Z]#PF[Int],_] @unchecked => sum(l.f)
    case r:R[_,Regular[Z]#PF[Int]] @unchecked => sum(r.g)
    case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int]) @unchecked => sum(star.f)+sum(star.g)
    case c:Comp[_,_,_] => 0 // ??
  }
 
  val t:TreeInt=NodeI(LeafI(1),LeafI(2))

  val l=List(1,2,3,4)

  println("sum of tree int = " + fold(t)(sum))
  println("sum of list int = " + fold(l)(sum))

  def count[Z]:Regular[Z]#PF[_]=>Int = {
      case U() => 0
      case k:K[_,Z] @unchecked=> 1
      case i:I[Int] @unchecked=> i.unI 
      case l:L[Regular[Z]#PF[_],_] @unchecked => count(l.f)
      case r:R[_,Regular[Z]#PF[_]] @unchecked => count(r.g)
      case star:(Regular[Z]#PF[_]:*:Regular[Z]#PF[_]) @unchecked => count(star.f)+count(star.g)
      case c:Comp[_,_,_] => 0 // ??
    }

  println("count of tree int = " + fold(t)(count))
  println("count of list A = " + fold(l)(count))


  def max[Z]:Regular[Z]#PF[Int]=>Int = {
      case U() => 0
      case k:K[Int,Z] @unchecked=> k.unK
      case i:I[Int] @unchecked=> i.unI 
      case l:L[Regular[Z]#PF[Int],_] @unchecked => max(l.f)
      case r:R[_,Regular[Z]#PF[Int]] @unchecked => max(r.g)
      case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int]) @unchecked => Math.max(max(star.f),max(star.g))
      case c:Comp[_,_,_] => 0 // ??
    }

  println("max of tree int = " + fold(t)(max))
  println("max of list A = " + fold(l)(max))
}