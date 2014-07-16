scala-reggen
============

Generic programming experiment in Scala

A category theory based approach to generic programming, where each datatype is described by a functor polynomial and defining a function used to fold each regular functor class.

In homage to Miles Sabin's awesome [Shapeless](https://github.com/milessabin/shapeless) this library should be eventually called "Useless"

TL;DR
-----
Since a fold can be interpreted as replacing a type's constructors with functions and any type can be described with an algebra of functors, by defining our functions over the elements of the algebra, we can write programs that are independent of the shape of the datatype. 


e.g.

List\[A\] ( Nil | Cons (a:A, l:List\[A\]) ) is represented as 1 + K A * I

Tree\[A\] (Leaf(a:A) |  Node(l:Tree\[A\],r:Tree\[A\])) is represented as K A + I * I




Background
----------

### A 100 mph tour into Category theory land

This library uses category theory as the basis for its machinery, so to understand how it works, we need some theory.

NOTE: This is a quick and far from rigorous pass, the theory behind this is explained in [Pardo, Alberto - Fundamentals of Generic Programming](http://www.fing.edu.uy/inco/cursos/proggen/Papers/FGP.pdf.gz)

[Alissa Pajer](http://alissapajer.github.io/conferenceslides/craftconf2014/#/) does a great introduction to the basic concepts and how they relate to programming

### Categories
A category is a collection of objects and arrows between them with identity and composition.
More formally:

Obj(Cat): collection of objects.
For each A,B ∈ Obj(Cat), there’s a set C(A,B) of morphisms (arrows) from A to B
f:A→B means f ∈ C(A,B)

Composition operation between arrows:
if f:A→B and g:B→C, then g∘f:A→C
Must be associative: f∘(g∘h) = (f∘g)∘h

For each object X, exists an identity arrow IdX:X→X, such as IdB∘f=f∘IdA=f for each f:A→B

In other words, to effectively form a Category, the choosen arrow composition must be associative and itmust be a unique 

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
