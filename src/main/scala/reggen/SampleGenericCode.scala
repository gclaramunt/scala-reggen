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
    case k:K[Int,Z] => k.unK
    case i:I[Int] => i.unI
    case l:L[Regular[Z]#PF[Int],_]  => sum(l.f)
    case r:R[_,Regular[Z]#PF[Int]]  => sum(r.g)
    case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int])  => sum(star.f)+sum(star.g)
  }
  
  println("sum of TreeInt = " + fold(ti)(sum))
  println("sum of Tree[Int] = " + fold(tp)(sum))
  println("sum of List[Int] = " + fold(l)(sum))
  println

  def count[Z]:Regular[Z]#PF[Int]=>Int = {
      case U() => 0
      case k:K[Int,Z] => 1
      case i:I[Int] => i.unI 
      case l:L[Regular[Z]#PF[Int],_]  => count(l.f)
      case r:R[_,Regular[Z]#PF[Int]]  => count(r.g)
      case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int])  => count(star.f)+count(star.g)
      //case c:Comp[_,_,_] => 0 
    }
  println("count of TreeInt = " + fold(ti)(count))
  println("count of Tree[Int] = " + fold(tp)(count))
  println("count of List[Int] = " + fold(l)(count))
  println

  def max[Z]:Regular[Z]#PF[Int]=>Int = {
      case U() => 0
      case k:K[Int,Z] => k.unK
      case i:I[Int] => i.unI 
      case l:L[Regular[Z]#PF[Int],_]  => max(l.f)
      case r:R[_,Regular[Z]#PF[Int]]  => max(r.g)
      case star:(Regular[Z]#PF[Int]:*:Regular[Z]#PF[Int])  => Math.max(max(star.f),max(star.g))
      //case c:Comp[_,_,_] => 0 
    }
  println("max of TreeInt = " + fold(ti)(max))
  println("max of Tree[Int] = " + fold(tp)(max))
  println("max of List[Int] = " + fold(l)(max))
  println

  def flatten[Z]:Regular[Z]#PF[_]=>List[_] = {
      case U() => Nil
      case k:K[_,Z] => List(k.unK)
      case i:I[List[_]] => i.unI 
      case l:L[Regular[Z]#PF[_],_]  =>  flatten(l.f)
      case r:R[_,Regular[Z]#PF[_]]  => flatten(r.g)
      case star:(Regular[Z]#PF[_]:*:Regular[Z]#PF[_])  => flatten(star.f)++flatten(star.g)
      //case c:Comp[_,_,_] => Nil 
    }
  println("flatten of TreeInt = " + fold(ti)(flatten))
  println("flatten of Tree[Int] = " + fold(tp)(flatten))
  println("flatten of List[Int] = " + fold(l)(flatten))
  println


  def serialize[Z]:Regular[Z]#PF[_]=>String = {
      case U() => "()"
      case k:K[_,Z] => k.unK.toString
      case i:I[String] => i.unI
      case l:L[Regular[Z]#PF[String],_]  => serialize(l.f)
      case r:R[_,Regular[Z]#PF[String]]  => serialize(r.g)
      case star:(Regular[Z]#PF[String]:*:Regular[Z]#PF[String])  => serialize(star.f)+","+serialize(star.g)
    }
  println("serialize of TreeInt = " + fold(ti)(serialize))
  println("serialize of Tree[Int] = " + fold(tp)(serialize))
  println("serialize of List[Int] = " + fold(l)(serialize))
  

  val tpbool:Tree[Boolean]=Node(Leaf(true),Leaf(false))
  val lbool=List(true,true, false, true)

  def all[Z]:Regular[Z]#PF[Boolean]=>Boolean = {
      case U() => true
      case k:K[Boolean,Z] => k.unK
      case i:I[Boolean] => i.unI 
      case l:L[Regular[Z]#PF[Boolean],_]  =>  all(l.f)
      case r:R[_,Regular[Z]#PF[Boolean]]  => all(r.g)
      case star:(Regular[Z]#PF[Boolean]:*:Regular[Z]#PF[Boolean])  => all(star.f) && all(star.g)
      //case c:Comp[_,_,_] => Nil 
    }  

  def exists[Z]:Regular[Z]#PF[Boolean]=>Boolean = {
      case U() => false
      case k:K[Boolean,Z] => k.unK
      case i:I[Boolean] => i.unI 
      case l:L[Regular[Z]#PF[Boolean],_]  =>  exists(l.f)
      case r:R[_,Regular[Z]#PF[Boolean]]  => exists(r.g)
      case star:(Regular[Z]#PF[Boolean]:*:Regular[Z]#PF[Boolean])  => exists(star.f) || exists(star.g)
      //case c:Comp[_,_,_] => Nil 
    }

  
  println("all tpbool = " + fold(tpbool)(all))
  println("exists tpbool = " + fold(tpbool)(exists))

  println("all lbool = " + fold(lbool)(all))
  println("exists lbool = " + fold(lbool)(exists))
 

}