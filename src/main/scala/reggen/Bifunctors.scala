package reggen

import Functors._
import Regular2Bifunctors._

/*
-- | Bifunctores regulares. Al igual que para los (mono)functores regulares
-- | definiremos los bifunctores regulares en Haskell y sus correspondientes
-- | instancias de la clase Bifunctor.

class Bifunctor f where
   bimap :: (a -> b) -> (r -> s) -> f a r -> f b s

   infixr 6 :++:
   infixr 7 :**:
*/


trait Bifunctor[F[_,_]] {
  def bimap[A, B, R, S](fa: F[A,R])(f: A => B,g:R=>S): F[B,S]
}

//combines two bifunctors with a bifunctor using the same two parameters
trait BiFComb[F[_,_],G[_,_],K[_,_]]{
  type abs[A,B]= K[F[A,B],G[A,B]]
}

object Bifunctors {

  /*
  -- | Bifunctor constante

   data K2 a b r = K2 a

   instance Bifunctor (K2 a)  where
      bimap f g (K2 x) = (K2 x)
  */

  case class K2[A,B,R](unK:A)

  implicit def FK2[A] =new Bifunctor[({type λ[B,R]=K2[A,B,R]})#λ]{
    def bimap[A1,B1,A2,B2](ka: K2[A,A1,A2])(f: A1 => B1, g: A2 => B2): K2[A,B1,B2]=K2(ka.unK)
  }

/*
      -- | Bifunctor constante 1

      data U2 a r = U2

      instance Bifunctor U2  where
         bimap f g U2 = U2
*/

  case class U2[A,R]()

  implicit def FU2 =new Bifunctor[U2]{
    def bimap[A1,B1,A2,B2](ua: U2[A1,A2])(f: A1 => B1, g: A2 => B2):U2[B1,B2]=U2()
  }

/*
          -- | Primera proyeccion (ocurrencia parametro)

         data Par a r = Par a

         instance Bifunctor Par where
            bimap f g (Par x) = Par (f x)


 */
  case class Par[A,R](unPar:A)

  implicit def FPar =new Bifunctor[Par]{
    def bimap[A1,B1,A2,B2](para:Par[A1,A2])(f: A1 => B1, g: A2 => B2):Par[B1,B2]=Par(f(para.unPar))
  }

/*
   -- | Segunda proyeccion (ocurrencia posicion recursiva)

            data Rec a r = Rec r

            instance Bifunctor Rec where
               bimap f g (Rec r) = Rec (g r)
*/
  case class Rec[A,R](unRec:R)

  implicit def FRec =new Bifunctor[Rec]{
    def bimap[A1,B1,A2,B2](reca:Rec[A1,A2])(f: A1 => B1, g: A2 => B2):Rec[B1,B2]=Rec(g(reca.unRec))
  }

/*
  -- | Suma de bifunctores

               data (f :++: g) a r = LL (f a r) | RR (g a r)

               instance (Bifunctor t, Bifunctor u) => Bifunctor (t :++: u) where
                  bimap f g (LL x) = LL (bimap f g x)
                     bimap f g (RR y) = RR (bimap f g y)
*/
/*
 //I can reuse from Functor.scala
  trait :+:[F,G] 
  case class L[F,G](f:F) extends :+:[F,G]
  case class R[F,G](g:G) extends :+:[F,G]
*/

implicit def bifplus[F[_,_], G[_,_]](implicit ff:Bifunctor[F], fg:Bifunctor[G])=new Bifunctor[BiFComb[F,G,:+:]#abs]{
  def bimap[A,B,A2,B2](fga:F[A,A2]:+:G[A,A2])(f: A => B, g:A2=>B2):F[B,B2]:+:G[B,B2]= fga match {
    case L(lf) => L(ff.bimap(lf)(f,g))
    case R(rg) => R(fg.bimap(rg)(f,g))
  }
}
/*
// I can reuse from Functor.scala
case class :*:[F,G](f:F,g:G) 
*/
implicit def fibifstar[F[_,_], G[_,_]](implicit ff:Bifunctor[F], fg:Bifunctor[G])=new Bifunctor[BiFComb[F,G,:*:]#abs]{
  def bimap[A,B,A2,B2](fga:F[A,A2]:*:G[A,A2])(f: A => B,g:A2=>B2):F[B,B2]:*:G[B,B2]= :*:(ff.bimap(fga.f)(f,g),fg.bimap(fga.g)(f,g))
}
/*
-- | Functor de tipo compuesto con bifunctor

data (d :@@: f) a r = Comp2 {unComp2 :: d (f a r)}

instance (Regular2 d, Bifunctor (PF2 d), Bifunctor f) => Bifunctor (d :@@: f) where
   bimap f g x = Comp2 $ pmap (bimap f g) $ unComp2 x


*/

  case class :@@:[D[_],F](unComp2:D[F])  

  implicit def fbicomp[F[_,_],D[_]](implicit ff:Bifunctor[F], rd:Regular2[D])=new Bifunctor[({ type abs[A,B]=D:@@:F[A,B]})#abs]{
    def bimap[A,B,A2,B2](fga:D:@@:F[A,A2])(f: A => B,g: A2=>B2):D:@@:F[B,B2]= :@@:(pmap(fga.unComp2)(ff.bimap(_)(f,g)))
  }

}
