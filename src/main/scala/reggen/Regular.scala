package reggen

import scala.language.higherKinds
import scala.language.existentials //double check 
import scala.language.implicitConversions

//non-parametric regular types
object Regular {

  /*
-- | *******************
-- | ** Class Regular **
-- | *******************

-- | Primeramente veamos la definicion de los functores regulares
-- | y su correspondiente instancia de la clase Functor.
-- |
-- | class Functor f where
-- |   fmap :: (a -> b) -> (f a -> f b)
*/

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}


/*

infixr 6 :+:
infixr 7 :*:

-- | Functor constante

newtype K a r = K { unK :: a }

instance Functor (K a) where
  fmap _ (K a) = K a
*/

case class K[A,R](unK:A)//  extends PF[R]

implicit def FK[A] =new Functor[({type λ[B]=K[A,B]})#λ]{
  def fmap[A1, B1](ka: K[A,A1])(f: A1 => B1): K[A,B1]=K(ka.unK)
}

/*
-- | Functor constante 1 (equivale a K ())

data U r = U

instance Functor U where
  fmap _ U = U
*/
case class U[R]()//  extends PF[R]

implicit def FU =new Functor[U]{
  def fmap[A, B](ua: U[A])(f: A => B):U[B]=U()
}

/*


-- | Functor identidad

newtype I r = I { unI :: r }

instance Functor I where
  fmap f (I r) = I (f r)

*/
case class I[R](unI:R)//  extends PF[R]
implicit def FId =new Functor[I]{
  def fmap[A, B](ida: I[A])(f: A => B):I[B]=I(f(ida.unI))
}

/*

-- | Suma de functores

data (f :+: g) r = L (f r) | R (g r)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L x) = L (fmap f x)
  fmap f (R y) = R (fmap f y)

*/

trait :+:[F,G] // extends PF[R]

case class L[F,G](f:F) extends :+:[F,G]
case class R[F,G](g:G) extends :+:[F,G]

trait FunctorCoprod[F[_],G[_]]{
  type abs[A]= F[A]:+:G[A]
}

implicit def fplus[A,F[_]:Functor, G[_]:Functor](implicit ff:Functor[F], fg:Functor[G])=new Functor[FunctorCoprod[F,G]#abs]{
  def fmap[A,B](fga:F[A]:+:G[A])(f: A => B):F[B]:+:G[B]= fga match {
    case L(lf) => L(ff.fmap(lf)(f))
    case R(rg) => R(fg.fmap(rg)(f))
  }
}


/*
-- : Producto de functores

data (f :*: g) r = f r :*: g r

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (x :*: y) = fmap f x :*: fmap f y

*/
case class :*:[F,G](f:F,g:G) // extends PF[R]

trait FunctorProd[F[_],G[_]]{
  type abs[A]= F[A]:*:G[A]
}

implicit def fstar[A,F[_]:Functor, G[_]:Functor](implicit ff:Functor[F], fg:Functor[G])=new Functor[FunctorProd[F,G]#abs]{
  def fmap[A,B](fga:F[A]:*:G[A])(f: A => B):F[B]:*:G[B]= :*:(ff.fmap(fga.f)(f),fg.fmap(fga.g)(f))
}

/*
-- | Composicion de functores

newtype (f :@: g) r = Comp {unComp :: f (g r)}

instance (Functor f, Functor g) => Functor (f :@: g) where
  fmap f r = Comp $ fmap (fmap f) $ unComp r
*/
case class Comp[F[_],G[_],R](unComp:F[G[R]]) // extends PF[R] 

implicit def fcomp[A,F[_]:Functor, G[_]:Functor](implicit ff:Functor[F], fg:Functor[G])=new Functor[({ type abs[A]=Comp[F,G,A]})#abs]{
  def fmap[A,B](fga:Comp[F,G,A])(f: A => B):Comp[F,G,B]= Comp(ff.fmap(fga.unComp)(fg.fmap(_)(f)))
}

/*

-- | Dado un tipo de dato T con functor base F, su representacion en terminos
-- | de la clase Regular es a traves de dos funciones from :: T -> F T y
-- | to :: F T -> T, que son las funciones testigos del isomorfismo T ~ F T.

-- | La representacion del functor base de un tipo de dato (tambien llamado
-- | "pattern functor") se hace a traves de lo que en Haskell se denomina
-- | una familia de tipos (algo asi como la definicion de una "funcion" a
-- | nivel de tipos). Dado un tipo "a", PF retorna un constructor de tipo
-- | de kind * -> * el cual correponde a la representaion Haskell del functor
-- | del tipo "a".

type family PF a :: * -> *

class Regular a where
   from :: a -> (PF a) a
   to   :: (PF a) a -> a
*/
/*
trait PF[T]{
  //todo: type constructor
  type F
  def apply(t:T):F
  def apply(pf:F):T
}
*/
trait Regular[T]{
  type PF[_]
  def from(t:T):PF[T]
  def to(pf:PF[T]):T
}

/*
-- | Ejemplo: Veamos la definicion del tipo TreeInt (arboles binarios de enteros)
-- | en terminos de la clase Regular.

data TreeInt = LeafI Int | NodeI TreeInt TreeInt
  deriving Show
*/
trait TreeInt{}
case class LeafI(i:Int) extends TreeInt
case class NodeI(l:TreeInt,r:TreeInt) extends TreeInt
/*
-- | La definicion del functor base de TreeInt se hace mediante la introduccion
-- | de una instancia de la familia PF.
-- | Corresponde al functor: F A = Int + A x A.

type instance PF TreeInt = K Int :+: (I :*: I)
*/
/*
 trait PFTreeInt extends PF[TreeInt]{
  type F[R]= K[Int,R]:+:(I[R]:*:I[R])
 }
*/
/*


class Regular TreeInt where
   from :: TreeInt -> (PF TreeInt) TreeInt
   to   :: (PF TreeInt) TreeInt -> TreeInt

-- | Definicion de la instancia de Regular para TreeInt.

instance Regular TreeInt where
  from (LeafI n)   = L (K n)
  from (NodeI x y) = R (I x :*: I y)

  to (L (K n))         = LeafI n
  to (R (I x :*: I y)) = NodeI x y
  */

  implicit def regularTreeInt:Regular[TreeInt]=new Regular[TreeInt]{

    type PF[R] = K[Int,R]:+:(I[R]:*:I[R])

    def from(t:TreeInt):PF[TreeInt] = t match {
      case LeafI(n) => L(K(n))
      case NodeI(x,y) => R(:*:(I(x),I(y)))
    }

    def to(pf:PF[TreeInt]):TreeInt=  pf match {
      case L(K(n)) => LeafI(n)
      case R(:*:(I(x),I(y))) => NodeI(x,y)
    }
  }

  /*

-- | Tipos parametricos tambien se pueden representar mediante la clase Regular.
-- | Como se vio en el curso, esto se hace usando un functor parametrizado.
-- | Veamos como ejemplo el tipo "List a" de las listas de tipo a.

data List a = Nil | Cons a (List a)
  deriving Show

-- | Definicion del functor base. Corresponde al functor L_A B = 1 + A x B.

type instance PF (List a) = U :+: (K a :*: I)

instance Regular (List a) where
  from Nil         = L U
  from (Cons x xs) = R (K x :*: I xs)

  to (L U)              = Nil
  to (R (K x :*: I xs)) = Cons x xs
*/

implicit def regularList[A]:Regular[List[A]]=new Regular[List[A]]{

    type PF[R] = U[R]:+:(K[A,R]:*:I[R])

    def from(t:List[A]):PF[List[A]] = t match {
      case Nil => L(U())
      case x::xs => R(:*:(K(x),I(xs)))
    }

    def to(pf:PF[List[A]]):List[A]=  pf match {
      case L(U()) => Nil
      case R(:*:(K(x),I(xs))) => x::xs
    }
  }

/*
-- | Definicion generica del operador fold.
-- | Esta definicion es valida para todo tipo "d" que sea instancia de Regular
-- | y cuyo functor "PF d" sea instancia de Functor.

fold  :: (Regular d, Functor (PF d)) => (PF d a -> a) -> d -> a
fold h = h . fmap (fold h) . from

-- | Ejemplos

sumTreeInt :: TreeInt -> Int
sumTreeInt = fold h
  where
    h (L (K n))         = n
    h (R (I x :*: I y)) = x + y

sumList :: Num a => List a -> a
sumList = fold h
  where
    h (L U)             = 0
    h (R (K n :*: I m)) = n + m
  */
  
}