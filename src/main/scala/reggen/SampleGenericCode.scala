package reggen

import Bifunctors._
import Regular._
import Regular2._
import SampleRegularDatatypes._
import reggen.RegFunctors._


object SampleGenericCode extends App {

  val ti:TreeInt=NodeI(LeafI(1),LeafI(2))
  val tp:Tree[Int]=Node(Node(Leaf(3),Node(Leaf(5),Leaf(7))),Node(Leaf(1),Leaf(2)))
  val l=List(1,2,3,4,1,9,4)

  def sumb[Z](r:Regular[Z]#PF[Int]):Int = r match {
    case U() => 0
    case K(x) => x
    case I(i) => i
    case L(f)  => sumb(f)
    case R(g)  => sumb(g)
    case :*:(f,g)  => sumb(f)+sumb(g)
    case _ => 0
  }


  println("sum of TreeInt = " + fold(ti)(sumb))
  println("sum of Tree[Int] = " + fold(tp)(sumb))
  println("sum of List[Int] = " + fold(l)(sumb))
  println
/*

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
      case k:K[Int,Z]  => k.unK
      case i:I[Int]   => i.unI 
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
    println(s"bool = $boolT" )
    println 

    println("all great than 4 of List[Int] = " + fold(boolL)(all) )
    println("all great than 4 of Tree[Int] = " + fold(boolT)(all) )
    println("exists one great than 4 of List[Int] = " + fold(boolL)(exists) )
    println("exists great than 4 of Tree[Int] = " + fold(boolT)(exists) )
    println 

  val rose = Rose(1, List(Rose(2,Nil), Rose(6,List(Rose(4,Nil),Rose(9,Nil))), Rose(3,Nil))) 


  def sum2[Z[_]:Regular2,X[_]]: Regular2[Z]#PF2[Int,Int] => Int =  {
    case U2() => 0
    case K2(_) => 0 //k2:K2[Int,_,Z[Int]] @unchecked => k2.unK
    case l:L[Regular2[Z]#PF2[Int,Int],_]  => sum2(implicitly[Regular2[Z]])(l.f)
    case r:R[_,Regular2[Z]#PF2[Int,Int]]  => sum2(implicitly[Regular2[Z]])(r.g)
    //case R(g)  => sum2(implicitly[Regular2[Z]])(g)
    case p:Par[Int,Int]  => p.unPar
    case r:Rec[Int,Int]  => r.unRec// sum2((implicitly[Regular2[Z]]).from2(r.unRec)) 
    case star:(Regular2[Z]#PF2[Int,Int]:*:Regular2[Z]#PF2[Int,Int])  => sum2(implicitly[Regular2[Z]])(star.f) + sum2(implicitly[Regular2[Z]])(star.g)
    case comp2:( :@@:[X, Regular2[X]#PF2[Int,Int]])  => {
      implicit val evX = comp2.rd
      val sum2app =sum2(evX)
      val newX = pmap(comp2.unComp2)(sum2app)(evX)
      fold2(newX)(sum2app)(comp2.rd)
    }

  }

  println("sum2 of Tree[Int] = " + fold2(tp)(sum2))
  println("sum2 of List[Int] = " + fold2(l)(sum2))
  println("sum2 of Rose[Int] = " + fold2(rose)(sum2))
*/

/*

psum :: (Regular2 d, Bifunctor (PF2 d), C_fsum (PF2 d), Num a) => d a -> a
psum = fold2 fsum
 */
/*
  def pfsum[Z[_]:Regular2,X[_]](x: Regular2[Z]#PF2[Int,Int]): Int = fsum(x) 

  def fsum[F[_,_]:Sum](f:F[Int,Int])=implicitly[Sum[F]].sum(f)

  trait Sum[BF[_,_]] {
    def sum(bf:BF[Int,Int]):Int
  }

  implicit val sumU2 = new Sum[U2]{
    def sum(u2:U2[Int,Int]):Int = 0 
  }

  implicit def sumK2[B] ={
    type BF[A,R]=K2[A,B,R]
    new Sum[BF]{
      def sum(k2:K2[Int,B,Int]):Int = k2.unK 
    }
  }

  implicit val sumPar = new Sum[Par]{
      def sum(par:Par[Int,Int]):Int = par.unPar
    }

  implicit val sumRec = new Sum[Rec]{
      def sum(rec:Rec[Int,Int]):Int = rec.unRec
    }

  //type BF[F[_,_],G[_,_]]= 

  implicit def sumL[F[_,_]:Sum,G[_,_]:Sum] =
    new Sum[BiFComb[F,G,:+:]#abs]{
      def sum(ss:F[Int,Int]:+:G[Int,Int]):Int = ss match {
        case L(l) => implicitly[Sum[F]].sum(l) 
        case R(r) => implicitly[Sum[G]].sum(r) 
    }
  }

  implicit def sumStar[F[_,_]:Sum,G[_,_]:Sum]= new Sum[BiFComb[F,G,:*:]#abs]{
      def sum(str:F[Int,Int] :*: G[Int,Int]):Int =  implicitly[Sum[F]].sum(str.f) +  implicitly[Sum[G]].sum(str.g)
    }
*/
}



object SampleWithMonoid extends App{


  trait Monoid[T]{
    val zero:T
    def append(t1:T, t2:T):T
  }

  def reduce[Z,T](r:RegFunctor{ type A=T })(implicit m:Monoid[T]):T = r match {
    case U() => m.zero
    case K(x) => x
    case I(i)  => i
    case L(f) => reduce(f)(m)
    case R(g) => reduce(g)(m)
    case f:*:g => m.append(reduce(f)(m),reduce(g)(m))
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

  val rose = Rose(1, List(Rose(2,Nil), Rose(6,List(Rose(4,Nil),Rose(9,Nil))), Rose(3,Nil))) 

/*
  nt]def reduce2[T,Z[_],X[_]](rr: Regular2[Z]#PF2[T,T])(implicit r2:Regular2[Z], m:Monoid[T]): T = rr match  {
    case U2() => m.zero 
    case k:K[T,_]=> k.unK
    case L(f) => reduce2(r2,m)(f)
    case r:R[_,Regular2[Z]#PF2[T,T]] @unchecked => reduce2(r.g)
    case p:Par[T,T] @unchecked => p.unPar
    case r:Rec[T,T] @unchecked => r.unRec// sum2((implicitly[Regular2[Z]]).from2(r.unRec)) 
    case star:(Regular2[Z]#PF2[T,T]:*:Regular2[Z]#PF2[T,T]) @unchecked => m.append(reduce2(star.f),reduce2(star.g))
    case comp2:( :@@:[X, Regular2[X]#PF2[T,T]]) @unchecked => {
      implicit val evX = comp2.rd
      val sum2app =x => reduce2(x)(evX,m) 
      val newX = pmap(comp2.unComp2)(sum2app)(evX)
      fold2(newX)(sum2app)(comp2.rd)
    }
  }
*/


}
