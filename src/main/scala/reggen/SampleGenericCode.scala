package reggen

import Functors._
import GenericFold._
import SampleRegularDatatypes._

object SampleGenericCode extends App {

  val ti:TreeInt=NodeI(LeafI(1),LeafI(2))
  val tp:Tree[Int]=Node(Leaf(1),Leaf(2))
  val l=List(1,2,3,4)

  def sum[Z]:Regular[Z]#PF[Int]=>Int = {
    case U() => 0
    case k:K[Int,Z] @unchecked=> k.unK
    case i:I[Int] @unchecked=> i.unI
    case l:L[Regular[Z]#PF[Int],_] @unchecked => sum(l.f)
    case r:R[_,Regular[Z]#PF[Int]] @unchecked => sum(r.g)
    case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int]) @unchecked => sum(star.f)+sum(star.g)
  }
  println("sum of TreeInt = " + fold(ti)(sum))
  println("sum of Tree[Int] = " + fold(tp)(sum))
  println("sum of List[Int] = " + fold(l)(sum))
  println

  def count[Z]:Regular[Z]#PF[_]=>Int = {
      case U() => 0
      case k:K[_,Z] @unchecked=> 1
      case i:I[Int] @unchecked=> i.unI 
      case l:L[Regular[Z]#PF[_],_] @unchecked => count(l.f)
      case r:R[_,Regular[Z]#PF[_]] @unchecked => count(r.g)
      case star:(Regular[Z]#PF[_]:*:Regular[Z]#PF[_]) @unchecked => count(star.f)+count(star.g)
      //case c:Comp[_,_,_] => 0 
    }
  println("count of TreeInt = " + fold(ti)(count))
  println("count of Tree[Int] = " + fold(tp)(count))
  println("count of List[Int] = " + fold(l)(count))
  println

  def max[Z]:Regular[Z]#PF[Int]=>Int = {
      case U() => 0
      case k:K[Int,Z] @unchecked=> k.unK
      case i:I[Int] @unchecked=> i.unI 
      case l:L[Regular[Z]#PF[Int],_] @unchecked => max(l.f)
      case r:R[_,Regular[Z]#PF[Int]] @unchecked => max(r.g)
      case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int]) @unchecked => Math.max(max(star.f),max(star.g))
      //case c:Comp[_,_,_] => 0 
    }
  println("max of TreeInt = " + fold(ti)(max))
  println("max of Tree[Int] = " + fold(tp)(max))
  println("max of List[Int] = " + fold(l)(max))
  println

  def flatten[Z]:Regular[Z]#PF[_]=>List[_] = {
      case U() => Nil
      case k:K[_,Z] @unchecked=> List(k.unK)
      case i:I[List[_]] @unchecked=> i.unI 
      case l:L[Regular[Z]#PF[_],_] @unchecked =>  flatten(l.f)
      case r:R[_,Regular[Z]#PF[_]] @unchecked => flatten(r.g)
      case star:(Regular[Z]#PF[_]:*:Regular[Z]#PF[_]) @unchecked => flatten(star.f)++flatten(star.g)
      //case c:Comp[_,_,_] => Nil 
    }
  println("flatten of TreeInt = " + fold(ti)(flatten))
  println("flatten of Tree[Int] = " + fold(tp)(flatten))
  println("flatten of List[Int] = " + fold(l)(flatten))
  println

  def serialize[Z]:Regular[Z]#PF[_]=>String = {
      case U() => ""
      case k:K[_,Z] @unchecked=> k.unK.toString
      case i:I[String] @unchecked=> i.unI
      case l:L[Regular[Z]#PF[String],_] @unchecked => serialize(l.f)
      case r:R[_,Regular[Z]#PF[String]] @unchecked => serialize(r.g)
      case star:(Regular[Z]#PF[String]:*:Regular[Z]#PF[String]) @unchecked => serialize(star.f)+","+serialize(star.g)
    }
  println("serialize of TreeInt = " + fold(ti)(serialize))
  println("serialize of Tree[Int] = " + fold(tp)(serialize))
  println("serialize of List[Int] = " + fold(l)(serialize))
  
  def forall[E,Z](q:E=>Boolean):Regular[Z]#PF[_]=>Boolean= {
      case U() => true
      case k:K[E,Z] @unchecked=> q(k.unK)
      case i:I[Boolean] @unchecked=> i.unI
      case l:L[Regular[Z]#PF[_],_] @unchecked => forall(q)(l.f)
      case r:R[_,Regular[Z]#PF[_]] @unchecked => forall(q)(r.g)
      case star:(Regular[Z]#PF[_]:*:Regular[Z]#PF[_]) @unchecked => forall(q)(star.f) && forall(q)(star.g)
    }

  def gtZero(x:Int)=x>0
  println("forall(_>0) of TreeInt = " + fold(ti)(forall( gtZero)))
  println("forall(_>0) of Tree[Int] = " + fold(tp)(forall( gtZero)))
  println("forall(_>0) of List[Int] = " + fold(l)(forall( gtZero)))
}