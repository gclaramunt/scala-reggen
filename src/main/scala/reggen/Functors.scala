package reggen

sealed trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

//combines two functors with a bifunctor using the same parameter
trait FComb[F[_], G[_], K[_,_]]{
  type abs[A]= K[F[A],G[A]]
}

object RegFunctors { type Aux[A] = RegFunctors { type T = A } }


trait RegFunctors {

  type A

  sealed trait RegFunctor

  case class K[R](unK:A) extends RegFunctor

  implicit def FK=new Functor[K]{
    def fmap[A1, B1](ka: K[A1])(f: A1 => B1): K[B1]=K(ka.unK) //does nothing??
  }

  case class U[R]() extends RegFunctor

  implicit def FU[A] =new Functor[U]{
    def fmap[A1, B1](ua: U[A1])(f: A1 => B1):U[B1]=U()
  }

  case class I[R](unI:R) extends RegFunctor
  implicit def FId[A] =new Functor[I]{
    def fmap[A1, B1](ida: I[A1])(f: A1 => B1):I[B1]=I(f(ida.unI))
  }

  sealed trait :+:[F <: RegFunctor ,G <: RegFunctor] extends RegFunctor

  case class L[F<: RegFunctor, G<: RegFunctor](f:F) extends :+:[F,G]
  case class R[F<: RegFunctor, G<: RegFunctor](g:G) extends :+:[F,G]


  implicit def fplus[F[_] <: RegFunctor, G[_] <: RegFunctor](implicit ff:Functor[F], fg:Functor[G])=new Functor[FComb[F,G,:+:]#abs]{
    def fmap[A1,B1](fga:F[A1]:+:G[A1])(f: A1 => B1):F[B1]:+:G[B1]= fga match {
      case L(lf) => L(ff.fmap(lf)(f))
      case R(rg) => R(fg.fmap(rg)(f))
    }
  }

  case class :*:[F <: RegFunctor , G <: RegFunctor ](f:F,g:G) extends RegFunctor

  implicit def fstar[F[_]<: RegFunctor, G[_]<: RegFunctor](implicit ff:Functor[F], fg:Functor[G])=new Functor[FComb[F,G,:*:]#abs]{
    def fmap[A1,B1](fga:F[A1]:*:G[A1])(f: A1 => B1):F[B1]:*:G[B1]= :*:(ff.fmap(fga.f)(f),fg.fmap(fga.g)(f))
  }

  case class :@:[F[_],G](unComp:F[G]) extends RegFunctor

  implicit def fcomp[F[_], G[_]](implicit ff:Functor[F], fg:Functor[G])=new Functor[({ type abs[X]=F:@:G[X]})#abs]{
    def fmap[A,B](fga:F:@:G[A])(f: A => B):F:@:G[B]= :@:(ff.fmap(fga.unComp)(fg.fmap(_)(f)))
  }
}
