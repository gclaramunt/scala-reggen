package reggen

sealed trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

//combines two functors with a bifunctor using the same parameter
trait FComb[F[_], G[_], K[_,_]]{
  type abs[A]= K[F[A],G[A]]
}

//object RegFunctors { type Aux[A] = RegFunctors { type T = A } }


object RegFunctors {

  sealed trait RegFunctor[A] {
    def fmap[B](f: A => B): RegFunctor[B]
  }

  case class K[A,R](unK:A) extends RegFunctor[A] {
    override def fmap[B](f: R => B): K[A,B] = K(unK)
  }


  case class U[A,R]() extends RegFunctor[A] {
    override def fmap[B](f: R => B): U[A,B] = U()
  }

  case class I[A,R](unI:R) extends RegFunctor[A] {
    override def fmap[B](f: R => B): I[A,B] = I(f(unI))
  }

  sealed trait :+:[X, F <: RegFunctor[_] ,G <: RegFunctor[_]] extends RegFunctor[X]

  case class L[X, F <: RegFunctor[X] ,G <: RegFunctor[X]](f:F) extends :+:[X,F,G] {
    override def fmap[B](h: (X) => B): L[B,F,G] = L(f.fmap(h))
  }
  case class R[X, F[_] <: RegFunctor[_] ,G[_] <: RegFunctor[_]](g:G[X]) extends :+:[X,F[X],G[X]] {
    override def fmap[B](h: (X) => B): L[B,F,G] = R(g.fmap(h))
  }

  case class :*:[X,F <: RegFunctor[_] , G <: RegFunctor[_] ](f:F,g:G) extends RegFunctor[X] {
    override def fmap[B](h: (X) => B): :*:[B,F,G] = :*:(f.fmap(h),g.fmap(h))
  }
/*
  case class :@:[X,F[_]:Functor , G <: RegFunctor[X] ](unComp:F[G]) extends RegFunctor[X] {
    override def fmap[B](f: (X) => B): RegFunctor[X]  = {
      val newComp = unComp.fmap(  )
      val inner =
      :@:(unComp.fmap((x:G) => x.fmap(f)))
    }
  }*/

}
