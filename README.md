scala-reggen
============

Generic programming experiment in Scala

A category theory based approach to generic programming, based on a pattern functor description of the type and defining a behavior for each regular functor class.

e.g.

List\[A\] ( Nil | Cons (a:A, l:List\[A\]) ) is represented as 1 + K A * I
Tree\[A\] (Leaf(a:A) |  Node(l:Tree\[A\],r:Tree\[A\])) is represented as K + I * I

In homage to Miles Sabin's awesome Shapeless[https://github.com/milessabin/shapeless] this library should be eventually called "Useless"