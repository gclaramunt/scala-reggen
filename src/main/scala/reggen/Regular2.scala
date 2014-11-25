package reggen

import scala.language.higherKinds


//parametric regular types
object Regular2Bifunctors {


trait Regular2[T[_]]{
  type PF2[_,_]
  val bf:Bifunctor[PF2]
  def from2[A](t:T[A]):PF2[A,T[A]]
  def to2[A](pf:PF2[A,T[A]]):T[A]
}
  /*
  -- | *********************************************
  -- | ** Definicion generica del functor de tipo **
  -- | *********************************************

  pmap :: (Regular2 d, Bifunctor (PF2 d)) => (a -> b) -> d a -> d b
  pmap f = to2 . bimap f (pmap f) . from2 
        == to2(bimap f (pmap f) from2 da )  
*/

  def pmap[A,B,D[_]](da:D[A])(f:A=>B)(implicit r2:Regular2[D]):D[B]={
    //bimap :: (a -> b) -> (r -> s) -> f a r -> f b s
    r2.to2(r2.bf.bimap(r2.from2(da))(f, (pmap(_)(f))))
  }

/*
  -- | *******************************************
  -- | ** Definicion generica del operador fold **
  -- | *******************************************

  fold2  :: (Regular2 d, Bifunctor (PF2 d)) => (PF2 d a b -> b) -> d a -> b
  fold2 h = h . bimap id (fold2 h) . from2
*/
  def fold2[A,B,D[_]](da:D[A])(h:Regular2[D]#PF2[A,B]=>B)(implicit r2:Regular2[D]):B=
    h(r2.bf.bimap(r2.from2(da))( identity ,fold2(_)(h)))

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
