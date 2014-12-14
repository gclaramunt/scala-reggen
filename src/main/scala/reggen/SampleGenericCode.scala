package reggen

import Functors._
import Regular2Bifunctors._
import GenericFold._
import SampleRegularDatatypes._

object SampleGenericCode extends App {

  val ti:TreeInt=NodeI(LeafI(1),LeafI(2))
  val tp:Tree[Int]=Node(Node(Leaf(3),Node(Leaf(5),Leaf(7))),Node(Leaf(1),Leaf(2)))
  val l=List(1,2,3,4,1,9,4)

  def sum[Z]:Regular[Z]#PF[Int]=>Int = {
    case U() => 0
    case k:K[Int,Z] @unchecked => k.unK
    case i:I[Int] @unchecked => i.unI
    case l:L[Regular[Z]#PF[Int],_] @unchecked  => sum(l.f)
    case r:R[_,Regular[Z]#PF[Int]] @unchecked  => sum(r.g)
    case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int]) @unchecked  => sum(star.f)+sum(star.g)
  }
  
  println("sum of TreeInt = " + fold(ti)(sum))
  println("sum of Tree[Int] = " + fold(tp)(sum))
  println("sum of List[Int] = " + fold(l)(sum))
  println

  def count[Z]:Regular[Z]#PF[Int]=>Int = {
      case U() => 0
      case k:K[Int,Z] @unchecked => 1
      case i:I[Int] @unchecked => i.unI 
      case l:L[Regular[Z]#PF[Int],_] @unchecked  => count(l.f)
      case r:R[_,Regular[Z]#PF[Int]] @unchecked  => count(r.g)
      case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int])  @unchecked => count(star.f)+count(star.g)
      //case c:Comp[_,_,_] => 0 
    }
  println("count of TreeInt = " + fold(ti)(count))
  println("count of Tree[Int] = " + fold(tp)(count))
  println("count of List[Int] = " + fold(l)(count))
  println

  def max[Z]:Regular[Z]#PF[Int]=>Int = {
      case U() => 0
      case k:K[Int,Z]  @unchecked => k.unK
      case i:I[Int]  @unchecked => i.unI 
      case l:L[Regular[Z]#PF[Int],_]  @unchecked => max(l.f)
      case r:R[_,Regular[Z]#PF[Int]]  @unchecked => max(r.g)
      case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int])  @unchecked => Math.max(max(star.f),max(star.g))
      //case c:Comp[_,_,_] => 0 
    }
  println("max of TreeInt = " + fold(ti)(max))
  println("max of Tree[Int] = " + fold(tp)(max))
  println("max of List[Int] = " + fold(l)(max))
  println

  def flatten[Z]:Regular[Z]#PF[_]=>List[_] = {
      case U() => Nil
      case k:K[_,Z]  @unchecked => List(k.unK)
      case i:I[List[_]] @unchecked => i.unI 
      case l:L[Regular[Z]#PF[_],_] @unchecked  =>  flatten(l.f)
      case r:R[_,Regular[Z]#PF[_]] @unchecked  => flatten(r.g)
      case star:(Regular[Z]#PF[_]:*:Regular[Z]#PF[_]) @unchecked  => flatten(star.f)++flatten(star.g)
      //case c:Comp[_,_,_] => Nil 
    }
  println("flatten of TreeInt = " + fold(ti)(flatten))
  println("flatten of Tree[Int] = " + fold(tp)(flatten))
  println("flatten of List[Int] = " + fold(l)(flatten))
  println


  def serialize[Z]:Regular[Z]#PF[_]=>String = {
      case U() => "()"
      case k:K[_,Z]  @unchecked => k.unK.toString
      case i:I[String] @unchecked => i.unI
      case l:L[Regular[Z]#PF[String],_] @unchecked  => serialize(l.f)
      case r:R[_,Regular[Z]#PF[String]] @unchecked  => serialize(r.g)
      case star:(Regular[Z]#PF[String]:*:Regular[Z]#PF[String]) @unchecked  => serialize(star.f)+","+serialize(star.g)
    }
  println("serialize of TreeInt = " + fold(ti)(serialize))
  println("serialize of Tree[Int] = " + fold(tp)(serialize))
  println("serialize of List[Int] = " + fold(l)(serialize))
  println 

  def all[Z]:Regular[Z]#PF[Boolean]=>Boolean = {
      case U() => true
      case k:K[Boolean,Z] @unchecked => k.unK
      case i:I[Boolean] @unchecked => i.unI 
      case l:L[Regular[Z]#PF[Boolean],_] @unchecked  =>  all(l.f)
      case r:R[_,Regular[Z]#PF[Boolean]] @unchecked  => all(r.g)
      case star:(Regular[Z]#PF[Boolean]:*:Regular[Z]#PF[Boolean]) @unchecked  => all(star.f) && all(star.g)
      //case c:Comp[_,_,_] => Nil 
    }  

  def exists[Z]:Regular[Z]#PF[Boolean]=>Boolean = {
      case U() => false
      case k:K[Boolean,Z] @unchecked => k.unK
      case i:I[Boolean] @unchecked => i.unI 
      case l:L[Regular[Z]#PF[Boolean],_] @unchecked  =>  exists(l.f)
      case r:R[_,Regular[Z]#PF[Boolean]] @unchecked  => exists(r.g)
      case star:(Regular[Z]#PF[Boolean]:*:Regular[Z]#PF[Boolean]) @unchecked  => exists(star.f) || exists(star.g)
      //case c:Comp[_,_,_] => Nil 
    }
    
    val boolL=pmap(l)(_>4)
    val boolT=pmap(tp)(_>4)

    println(s"boolL = $boolL" )
    println(s"boolT = $boolT" )
    println 
    

    println("all great than 4 of List[Int] = " + fold(boolL)(all) )
    println("all great than 4 of Tree[Int] = " + fold(boolT)(all) )
    println("exists one great than 4 of List[Int] = " + fold(boolL)(exists) )
    println("exists great than 4 of Tree[Int] = " + fold(boolT)(exists) )
    println 
}


object SampleWithMonoid extends App{


  trait Monoid[T]{
    val zero:T
    def append(t1:T, t2:T):T
  }

  def reduce[Z,T](implicit m:Monoid[T]):Regular[Z]#PF[T]=>T = {
    case U() => m.zero
    case k:K[T,Z] => k.unK
    
    case i:I[T]  => i.unI
    case l:L[Regular[Z]#PF[T],_]   => reduce(m)(l.f)
    case r:R[_,Regular[Z]#PF[T]]   => reduce(m)(r.g)
    case star:(Regular[Z]#PF[T]:*:Regular[Z]#PF[T])   => m.append(reduce(m)(star.f),reduce(m)(star.g))
  }

  val ti:TreeInt=NodeI(LeafI(1),LeafI(2))
  val tp:Tree[Int]=Node(Leaf(1),Leaf(2))
  val l=List(1,2,3,4)

  object MSum extends Monoid[Int] {
    val zero=0
    def append(t1:Int, t2:Int):Int =t1+t2
  }

  object MList extends Monoid[List[_]] {
    val zero=Nil
    def append(t1:List[_], t2:List[_]):List[_] =t1++t2
  }


}
