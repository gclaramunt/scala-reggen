scala-reggen
============

Generic programming experiment in Scala

A category theory based approach to generic programming, where each datatype is described by a functor polynomial and defining a function used to fold each regular functor class.

In homage to Miles Sabin's awesome [Shapeless](https://github.com/milessabin/shapeless) this library should be eventually called "Useless"

Sample code
-----------
( from [SampleGenericCode.scala](./blob/master/src/main/scala/reggen/SampleGenericCode.scala) )  
```
object SampleGenericCode extends App {

  val ti:TreeInt=NodeI(LeafI(1),LeafI(2))
  val tp:Tree[Int]=Node(Leaf(1),Leaf(2))
  val l=List(1,2,3,4)

  def sum[Z]:Regular[Z]#PF[Int]=>Int = {
    case U() => 0
    case k:K[Int,Z] => k.unK
    case i:I[Int] => i.unI
    case l:L[Regular[Z]#PF[Int],_]  => sum(l.f)
    case r:R[_,Regular[Z]#PF[Int]]  => sum(r.g)
    case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int])  => sum(star.f)+sum(star.g)
  }
  
  println("sum of TreeInt = " + fold(ti)(sum))
  println("sum of Tree[Int] = " + fold(tp)(sum))
  println("sum of List[Int] = " + fold(l)(sum))

}
```

TL;DR
-----
Since a fold can be interpreted as replacing a type's constructors with functions and any type can be described with an algebra of functors, by defining our functions over the elements of the algebra, we can write programs that are independent of the shape of the datatype. 
Our algebra of functors consists of:
1 (unit), K (constant), I (identity), * (product), + (sum)
 
e.g.

List\[A\] ( Nil | Cons (a:A, l:List\[A\]) ) is represented as 1 + K A * I

Tree\[A\] (Leaf(a:A) |  Node(l:Tree\[A\],r:Tree\[A\])) is represented as K A + I * I

Then a generic fold just takes a function that pattern matches over the components of the functors.

e.g. for the sum:
```
  sum :: F a -> int
  sum 1 = 0
  sum K a = a
  sum (inl x) = sum x -- left injection of sum
  sum (inr x) = sum x -- right injection of sum
  sum (x * y) = sum x + sum y
```

Background
----------

### A 100 mph tour into Category theory land

This library uses category theory as the basis for its machinery, so to understand how it works, we need some theory.

NOTE: This is a quick and far from rigorous pass, the theory behind this is explained in [Pardo, Alberto - Fundamentals of Generic Programming](http://www.fing.edu.uy/inco/cursos/proggen/Papers/FGP.pdf.gz)

[Alissa Pajer](http://alissapajer.github.io/conferenceslides/craftconf2014/#/) does a great introduction to the basic concepts and how they relate to programming

### Categories
A category is a collection of objects and arrows between them with identity and composition:

Obj(Cat): collection of objects.
For each A,B ∈ Obj(Cat), there’s a set C(A,B) of morphisms (arrows) from A to B
f:A→B means f ∈ C(A,B)

(Formally, a category is defined by 3 total functions: source, target, id;  and one partial function: composition)

Composition operation between arrows:
if f:A→B and g:B→C, then g∘f:A→C
Must be associative: f∘(g∘h) = (f∘g)∘h

For each object X, exists an identity arrow IdX:X→X, such as IdB∘f=f∘IdA=f for each f:A→B

In other words, to effectively form a Category, the choosen arrow composition must be associative and it must be unique 

### Functors
A functor from cat. C to cat. D is a pair of functions one maps objects of C to D and the other maps arrows of C to D
functor F:C→D, A ∈C⇒FA∈D, f ∈C⇒Ff∈D

A functor must satisfy the following laws:
Ff:FA→FB
F(f∘g)=Ff∘Fg
FidA=idFA

(TO BE DONE:)

### Initial Algebras
(how types and functions are a category)
(how type constructors )
(how types can be described as an algebra of functors)

### Folds

(define fold)
  (laws)

(how fold is the unversal operation)
