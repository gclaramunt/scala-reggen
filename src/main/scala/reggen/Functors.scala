package reggen

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functors {

  sealed trait PFn

  case class K[A, Z](unK: A) extends PFn

  implicit def FK[A]: Functor[K[A, ?]] = new Functor[K[A, ?]] {
    def fmap[A1, B1](ka: K[A, A1])(f: A1 => B1): K[A, B1] = K(ka.unK)
  }

  case class U[Z]() extends PFn

  implicit def FU: Functor[U] = new Functor[U] {
    def fmap[A, B](ua: U[A])(f: A => B): U[B] = U()
  }

  case class I[Z](unI: Z) extends PFn
  implicit def FId: Functor[I] = new Functor[I] {
    def fmap[A, B](ida: I[A])(f: A => B): I[B] = I(f(ida.unI))
  }

  trait :+:[F, G] extends PFn

  case class L[F, G](f: F) extends :+:[F, G]
  case class R[F, G](g: G) extends :+:[F, G]

  implicit def fplus[F[_], G[_]](
      implicit ff: Functor[F],
      fg: Functor[G]): Functor[Lambda[A => F[A] :+: G[A]]] =
    new Functor[Lambda[A => F[A] :+: G[A]]] {
      def fmap[A, B](fga: F[A] :+: G[A])(f: A => B): F[B] :+: G[B] = fga match {
        case L(lf) => L(ff.fmap(lf)(f))
        case R(rg) => R(fg.fmap(rg)(f))
      }
    }

  case class :*:[F, G](f: F, g: G) extends PFn

  implicit def fstar[F[_], G[_]](
      implicit ff: Functor[F],
      fg: Functor[G]): Functor[Lambda[A => F[A] :*: G[A]]] =
    new Functor[Lambda[A => F[A] :*: G[A]]] {
      def fmap[A, B](fga: F[A] :*: G[A])(f: A => B): F[B] :*: G[B] =
        :*:(ff.fmap(fga.f)(f), fg.fmap(fga.g)(f))
    }

  case class :@:[F[_], G](unComp: F[G]) extends PFn

  implicit def fcomp[F[_], G[_]](
      implicit ff: Functor[F],
      fg: Functor[G]): Functor[Lambda[A => F :@: G[A]]] =
    new Functor[Lambda[A => F :@: G[A]]] {
      def fmap[A, B](fga: F :@: G[A])(f: A => B): F :@: G[B] =
        :@:(ff.fmap(fga.unComp)(fg.fmap(_)(f)))
    }
}
