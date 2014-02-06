package reggen

import scala.language.higherKinds

object Regular {

/*
  -- | **********************************
  -- | ** Tipos regulares parametricos **
  -- | **********************************

  -- | Como visto en el curso, los tipos regulares parametricos pueden ser
  -- | representados en funcion de bifunctores regulares.
  -- | Para ello es necesario definir una nueva clase, que llamaremos
  -- | Regular2 y una nueva familia de tipos, PF2, que asocia un bifunctor
  -- | (representado por un constructor de tipo de kind * -> * -> *) a
  -- | a cada constructor t de kind * -> * que es declarado instancia de
  -- | la familia de tipos.
*/
  // type family PF2 (t :: * -> *) :: * -> * -> *
  // ej type instance PF2 List = U2 :++: (Par :**: Rec)
/*
  //NO ENCODING ????
  trait PF2[TT[_],RR[_,_]]

  implicit def pf2List = new PF2[List]{ type RR= :++:[U2,Par:**:Rec]}

  // class Regular2 t where
  //    from2 :: t a -> (PF2 t) a (t a)  --- recibe t a y devuelve el tipo resultante de aplicar a PF2(t) a y t a
  //    to2   :: (PF2 t) a (t a) ->  t a

  trait Regular2[T[_]]{
    def from2[A]:T[A]=>PF2[A,T] 
    def to2[A]:PF2[A,T]=>T[A]
  }
*/

  

  /*
    -- | Bifunctores regulares. Al igual que para los (mono)functores regulares
    -- | definiremos los bifunctores regulares en Haskell y sus correspondientes
    -- | instancias de la clase Bifunctor.

    class Bifunctor f where
       bimap :: (a -> b) -> (r -> s) -> f a r -> f b s
  */
/*  trait Bifunctor[F[_, _]] {
    def bimap[A,R,B,S](f: A => B, g: R => S): F[A, R]=>F[B, S]
  }
*/
  trait Bifunctor[F[_, _]] {
    def bimap[A, R, B, S](fa: F[A, R], f: A => B, g: R => S): F[B, S]
  }


  /*
      infixr 6 :++:
      infixr 7 :**:

    no way :(
  */

  /*
    -- | Bifunctor constante

    data K2 a b r = K2 a
  */
  case class K2[A,B,R](a:A) 

  /*
    instance Bifunctor (K2 a)  where
       bimap f g (K2 x) = (K2 x)
  */
  implicit def Bik2[A] =new Bifunctor[ ({type λ[B,R]=K2[A,B,R]})#λ ]{
    def bimap[A1, R, B, S](k2:K2[A,A1,R], f: A1 => B,g: R => S)= K2[A,B,S](k2.a) 
  }

  /*
    -- | Bifunctor constante 1

    data U2 a r = U2
  */
  case class U2[A,R]()
  /*
    instance Bifunctor U2  where
       bimap f g U2 = U2
  */
  implicit def BiU2 =new Bifunctor[U2]{
    def bimap[A, R, B, S](u2:U2[A,R], f: A => B,g: R => S)= U2[B,S]()
  }

  /*
    -- | Primera proyeccion (ocurrencia parametro)

    data Par a r = Par a
  */
  case class Par[A,R](a:A)
  /*
    instance Bifunctor Par where
       bimap f g (Par x) = Par (f x)
  */
  implicit def BiPar= new Bifunctor[Par]{
    def bimap[A, R, B, S]( par:Par[A,R], f: A => B,g: R => S)=  Par[B,S](f(par.a))
  }

  /*
    -- | Segunda proyeccion (ocurrencia posicion recursiva)

    data Rec a r = Rec r
  */
  case class Rec[A,R](r:R)
  /*
    instance Bifunctor Rec where
       bimap f g (Rec r) = Rec (g r)
  */
  implicit def BiRec = new  Bifunctor[Rec] {
    def bimap[A, R, B, S](rec:Rec[A,R], f: A => B,g: R => S)=  Rec[B,S](g(rec.r))
  }

  /*
    -- | Suma de bifunctores

    data (f :++: g) a r = LL (f a r) | RR (g a r)
  */

  //ugh! must be a better way
  trait :++:[A,R,F[_,_],G[_,_]]
  case class LL[A,R,F[_,_],G[_,_]](f:F[A,R]) extends :++:[A,R,F,G]
  case class RR[A,R,F[_,_],G[_,_]](g:G[A,R]) extends :++:[A,R,F,G]

    /*
      instance (Bifunctor t, Bifunctor u) => Bifunctor (t :++: u) where
         bimap f g (LL x) = LL (bimap f g x)
         bimap f g (RR y) = RR (bimap f g y)
    */

  implicit def BiPlus[F[_,_],G[_,_]](implicit bf:Bifunctor[F],bg:Bifunctor[G])=new Bifunctor[({type λ[A,R]= :++:[A,R,F,G]})#λ] {
    def bimap[A, R, B, S](fa: :++:[A,R,F,G], f: A => B, g: R => S): :++:[B,S,F,G]=fa match {
      case LL(lf) =>LL(bf.bimap(lf,f,g))
      case RR(rg) =>RR(bg.bimap(rg,f,g))
    }
  }


  /*
    -- | Producto de bifunctores

    data (f :**: g) a r = f a r :**: g a r
  */

  case class :**:[A,R,F[_,_],G[_,_]](f:F[A,R],g:G[A,R])

  /*
  instance (Bifunctor t, Bifunctor u) => Bifunctor (t :**: u) where
     bimap f g (x :**: y) = bimap f g x :**: bimap f g y
  */
  implicit def BiStar[F[_,_],G[_,_]](implicit bf:Bifunctor[F],bg:Bifunctor[G])=new Bifunctor[({type λ[A,R]= :**:[A,R,F,G]})#λ] {
    def bimap[A, R, B, S](fa: :**:[A,R,F,G], f: A => B, g: R => S): :**:[B,S,F,G]= :**:(bf.bimap(fa.f,f,g),bg.bimap(fa.g,f,g))
  }


  // type family PF2 (t :: * -> *) :: * -> * -> *
  // ej type instance PF2 List = U2 :++: (Par :**: Rec)
  /*
    //NO ENCODING ????
    trait PF2[TT[_],RR[_,_]]

    implicit def pf2List = new PF2[List]{ type RR= :++:[U2,Par:**:Rec]}

    // class Regular2 t where
    //    from2 :: t a -> (PF2 t) a (t a)  --- recibe t a y devuelve el tipo resultante de aplicar a PF2(t) a y t a
    //    to2   :: (PF2 t) a (t a) ->  t a

  */
  trait PF2[T[_,_]] {
    type INST
  }

  trait Regular2[T[_]]{
    def from2[A,PF2[_,_]]:T[A]=>PF2[A,T[A]] 
    def to2[A,PF2[_,_]]:PF2[A,T[A]]=>T[A]
  }


  type PARREC=:**:[A,R,Par[_,_],Rec[_,_]]
  type BF_LIST=U2:++:PARREC
  implicit def pf2OfList = new PF2[List]{ type INST= BF_LIST}
  //implicit def regOfList= new 

  //trait PF2[TT[_],RR[_,_]]
  /*
  trait PF2[T[_]]{
    type BF[_,_]

  }
*/
  /*
  -- | Functor de tipo compuesto con bifunctor

  data (d :@@: f) a r = Comp2 {unComp2 :: d (f a r)}
  */
  case class Comp2[A,R,F[A,R],D[F[A,R]]](unComp2:D[F]){
    type PF2D[X]=PF2[X,D[X]]
  }
  type :@@:[A,R,F[A,R],D[F[A,R]]] = Comp2[A,R,F,D]

/*
  trait CompApply[D[_],F[_,_],PF2[_,_]]{
    type PF2D[X]=PF2[X,D[X]]
    type BF[A,R]=Comp2[A,R,F,PF2D]
  }*/

  /*
  -- | *********************************************
  -- | ** Definicion generica del functor de tipo **
  -- | *********************************************

  pmap :: (Regular2 d, Bifunctor (PF2 d)) => (a -> b) -> d a -> d b
  pmap f = to2 . bimap f (pmap f) . from2 
        == to2(bimap f (pmap f) from2 da )  
*/
  def pmap[A,B,D[_]](da:D[A],f:A=>B)(implicit d:Regular2[D], pf2:PF2[D], bf:Bifunctor[DF2D]):D[B]={
    //bimap :: (a -> b) -> (r -> s) -> f a r -> f b s
    d.to2(bf.bimap(d.from2(da),f, (x:D[A])=>pmap(x,f)(d,bf)))
  }

  /*
  instance (Regular2 d, Bifunctor (PF2 d), Bifunctor f) => Bifunctor (d :@@: f) where
     bimap f g x = Comp2 $ pmap (bimap f g) $ unComp2 x
  */

  implicit def BiComp[F[_,_]:Bifunctor,D[_]:Regular2,PF2[_,_]:Bifunctor]=new Bifunctor[CompApply[D,F,PF2]#BF]{
    
    val bf=implicitly[Bifunctor[F]]
    val d=implicitly[Regular2[D]]
    def bimap[A, R, B, S](fa: Comp2[A,R,F,PF2D], f: A => B, g: R => S): Comp2[B,S,F,PF2D]= Comp2(
      pmap(fa.unComp2, (x:F[A,R]) => bf.bimap(x,f,g))(d,bf)
    )
  }

/*
 trait Bifunctor[F[_, _]] {
    def bimap[A, R, B, S](fa: F[A, R], f: A => B, g: R => S): F[B, S]
  }
*/


/*
  -- | *******************************************
  -- | ** Definicion generica del operador fold **
  -- | *******************************************

  fold2  :: (Regular2 d, Bifunctor (PF2 d)) => (PF2 d a b -> b) -> d a -> b
  fold2 h = h . bimap id (fold2 h) . from2
*/
  def fold2[A,B,D[A]:Regular2,PF2D[A,B]:Bifunctor](da:D[A],h:PF2D[A,B]=>B):B={
    val bf=implicitly[Bifunctor[PF2D]]
    val d=implicitly[Regular2[D]]
    h(bf.bimap(d.from2(da), identity ,((x:D[A])=>fold2(x,h)(d,bf))))
  }



/*
  -- Alternativamente:
  -- pmap f = fold2 (to2 . bimap f id)

  -- | Ejemplo: List
  -- | data List a = Nil | Cons a (List a)

  type instance PF2 List = U2 :++: (Par :**: Rec)

  instance Regular2 List where
    from2 Nil         = LL U2
    from2 (Cons x xs) = RR (Par x :**: Rec xs)

    to2 (LL U2)                  = Nil
    to2 (RR (Par x :**: Rec xs)) = Cons x xs

  sumList2 :: Num a => List a -> a
  sumList2 = fold2 h
    where
      h (LL U2)                 = 0
      h (RR (Par n :**: Rec m)) = n + m

  -- | Ejemplo: Arboles binarios con informacion en las hojas

  data Btree a = Leaf a | Join (Btree a) (Btree a) deriving Show

  type instance PF2 Btree = Par :++: (Rec :**: Rec)

  instance Regular2 Btree where
    from2 (Leaf a)   = LL (Par a)
    from2 (Join t u) = RR (Rec t :**: Rec u)

    to2 (LL (Par a))            = Leaf a
    to2 (RR (Rec t :**: Rec u)) = Join t u

  sumBtree :: Num a => Btree a -> a
  sumBtree = fold2 h
    where
      h (LL (Par a))            = a
      h (RR (Rec m :**: Rec n)) = m + n

  -- | Ejemplo: Rose trees

  data Rose a = Fork a (List (Rose a))

  type instance PF2 Rose = Par :**: (List :@@: Rec)

  instance Regular2 Rose where
    from2 (Fork a rs) = Par a :**: (Comp2 (pmap Rec rs))

    to2 (Par a :**: Comp2 rs) = Fork a (pmap unRec rs)
      where
        unRec (Rec r) = r

  sumRose :: Num a => Rose a -> a
  sumRose = fold2 h
    where
      h (Par a :**: Comp2 rs) = a + sumList2 (pmap unRec rs)
      unRec (Rec r) = r

  -- | **************************************
  -- | ** Definicion suma politipica: psum **
  -- | **************************************

  -- | La siguiente definicion de psum funciona para toda estructura "d"
  -- | que sea definida como instancia de la clase Regular2 y cuyo functor
  -- | "PF2 d" sea instancia de Bifunctor (este es un requerimiento de fold2).
  -- | Para que la definicion termine siendo politipica, el algebra fsum del fold
  -- | se define como el unico metodo de una clase que llamamos C_fsum. Luego,
  -- | se definen instancias de esta clase para cada bifunctor regular.
  -- | En el contexto de tipos de psum se debe agregar la restriccion de que
  -- | hay una instancia de C_fsum para el bifunctor "PF2 d" del tipo de dato d.

  -- | Esta es la "metodologia" que usaremos para definir funciones politipicas
  -- | asociadas a tipos de datos representables por Regular2. Es muy similar a
  -- | lo que internamente define PolyP 2.

  psum :: (Regular2 d, Bifunctor (PF2 d), C_fsum (PF2 d), Num a) => d a -> a
  psum = fold2 fsum

  class C_fsum f where
    fsum :: Num a => f a a -> a

  instance C_fsum (K2 a) where
    fsum _ = 0

  instance C_fsum U2 where
    fsum _ = 0

  instance C_fsum Par where
    fsum (Par a) = a

  instance C_fsum Rec where
    fsum (Rec r) = r

  instance (C_fsum f, C_fsum g) => C_fsum (f :++: g) where
    fsum (LL x) = fsum x
    fsum (RR y) = fsum y

  instance (C_fsum f, C_fsum g) => C_fsum (f :**: g) where
    fsum (x :**: y) = fsum x + fsum y

  -- | Las restricciones Regular2 d, Bifunctor (PF2 d), C_fsum (PF2 d) son
  -- | necesarias para poder invocar a psum sobre el tipo d.
  instance (Regular2 d, Bifunctor (PF2 d), C_fsum (PF2 d), C_fsum f)
           => C_fsum (d :@@: f) where
    fsum (Comp2 x) = psum . pmap fsum $ x

  -- | Ejemplos de llamadas

  lista = Cons 3 (Cons 4 Nil)
  arbol = Join (Leaf 5) (Leaf 6)
  rose  = Fork 4 (Cons (Fork 5 Nil) (Cons (Fork 6 Nil) Nil))

  psum1 = psum $ lista
  psum2 = psum $ arbol
  psum3 = psum $ rose

  -- | ***********************************
  -- | ** Definicion flatten politipico **
  -- | ***********************************

  flatten :: (Regular2 d, Bifunctor (PF2 d), C_fl (PF2 d)) => d a -> [a]
  flatten = fold2 fl

  class C_fl f where
    fl :: f a [a] -> [a]

  instance C_fl (K2 a) where
    fl _ = []

  instance C_fl U2 where
    fl _ = []

  instance C_fl Par where
    fl (Par a) = [a]

  instance C_fl Rec where
    fl (Rec r) = r

  instance (C_fl f, C_fl g) => C_fl (f :++: g) where
    fl (LL x) = fl x
    fl (RR y) = fl y

  instance (C_fl f, C_fl g) => C_fl (f :**: g) where
    fl (x :**: y) = fl x ++ fl y

  instance (Regular2 d, Bifunctor (PF2 d), C_fl (PF2 d), C_fl f)
           => C_fl (d :@@: f) where
    fl (Comp2 x) = concat . flatten . pmap fl $ x

  -- | ***************************************
  -- | ** Definicion size politipico: psize **
  -- | ***************************************

  psize :: (Regular2 d, Bifunctor (PF2 d), C_fsize (PF2 d)) => d a -> Int
  psize = fold2 fsize

  class C_fsize f where
    fsize :: f a Int -> Int

  instance C_fsize (K2 a) where
    fsize _ = 0

  instance C_fsize U2 where
    fsize _ = 0

  instance C_fsize Par where
    fsize (Par a) = 1

  instance C_fsize Rec where
    fsize (Rec r) = r

  instance (C_fsize f, C_fsize g) => C_fsize (f :++: g) where
    fsize (LL x) = fsize x
    fsize (RR y) = fsize y

  instance (C_fsize f, C_fsize g) => C_fsize (f :**: g) where
    fsize (x :**: y) = fsize x + fsize y

  instance (Regular2 d, Bifunctor (PF2 d), C_fsum (PF2 d), C_fsize f)
           => C_fsize (d :@@: f) where
    fsize (Comp2 x) = psum . pmap fsize $ x*/
}