import scala.language.higherKinds

/*
-----------------------------------------------------------------------------
-- Functorial structural representation types.
-----------------------------------------------------------------------------
*/

//TODO: use scalaz one
trait Functor[F[_]] { 
  def fmap[A, B](f: A => B)(fa: F[A]): F[B]
}

//TODO: use scalaz one
trait Bifunctor[F[_, _]]  {
  def bimap[A, B, C, D](f: A => C, g: B => D)(fab: F[A, B]): F[C, D]
}

/*
-- | Structure type for constant values.
newtype K a r    = K { unK :: a }
*/
case class K[A,_](unK:A)

/*
-- | Structure type for recursive values.
newtype I r      = I { unI :: r }
*/
case class I[R](unI:R)

/*
-- | Structure type for empty constructors.
data U r         = U
*/
case class U[-_]() //??? and btw, it really should be contravariant??

/*
Structure type for alternatives in a type.
data (f :+: g) r = L (f r) | R (g r)
*/
trait :+:[A,B]
case class L[A,B](f: A) extends :+:[A,B]
case class R[A,B](g: B) extends :+:[A,B]

/*
-- | Structure type for fields of a constructor.
data (f :*: g) r = f r :*: g r
*/
case class :*:[A,B](l:A,r:B)

/*
-- | Structure type to store the name of a constructor.
data C c f r =  C { unC :: f r }
*/
case class C[T](c:String,unC:T) // c=String ??????

/*
-- | Structure type to store the name of a record selector.
data S l f r =  S { unS :: f r }
*/
case class S[T](s:String,unS:T)

/*
-----------------------------------------------------------------------------
-- Functorial map function.
-----------------------------------------------------------------------------
*/
/*
instance Functor I where fmap f (I r) = I (f r)
*/
object IFunctor extends Functor[I]{
  def fmap[A, B](f: A => B)(ia: I[A]): I[B]=I(f(ia.unI))
}
/*
instance Functor (K a) where
  fmap _ (K a) = K a
*/
trait KFunctor[Z] extends Functor[({type H[X]=K[Z,X]})#H]{
  def fmap[A, B](f: A => B)(kz: K[Z,_]): K[Z,_]=kz
}

/*
instance Functor U where
  fmap _ U = U
*/
object UFunctor extends Functor[U]{
  def fmap[A, B](f: A => B)(u: U[A]): U[B]=U[B]()
}

/*
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap h (L x) = L (fmap h x)
  fmap h (R y) = R (fmap h y)
*/
trait CoproductFunctor[A,B] extends Bifunctor[:+:]{
  def bimap[A, B, C, D](f: A => C, g: B => D)(fab: A:+:B): C:+:D = fab match {
    case L(a)=>L(f(a))
    case R(a)=>R(g(a))
  }
}


/*
instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (x :*: y) = fmap f x :*: fmap f y
*/
trait ProductFunctor[A,B] extends Bifunctor[:*:]{
  def bimap[A, B, C, D](f: A => C, g: B => D)(fab: A:*:B): C:*:D =fab match { 
    case a:*:b => :*:(f(a),g(b)) 
  } 
}

/*
instance Functor f => Functor (C c f) where
  fmap f (C r) = C (fmap f r)
*/
object CFunctor extends Functor[C]{
  def fmap[A, B](f: A => B)(ca: C[A]): C[B]=C(ca.c,f(ca.unC))
}

/*
instance Functor f => Functor (S c f) where
  fmap f (S r) = S (fmap f r)
*/
object SFunctor extends Functor[S]{
  def fmap[A, B](f: A => B)(sa: S[A]): S[B]=S(sa.s,f(sa.unS))
}
