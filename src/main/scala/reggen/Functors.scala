package reggen

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

//combines two functors with a bifunctor using the same parameter
trait FComb[F[_],G[_],K[_,_]]{
    type abs[A]= K[F[A],G[A]]
  }

object Functors {

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
}