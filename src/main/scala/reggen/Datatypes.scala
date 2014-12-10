package reggen

import Functors._
import Bifunctors._
import Regular2Bifunctors._

/**
  Some simple data types and corresponding regular typeclasses
*/
object SampleRegularDatatypes{

  //simple Binary tree of ints
  trait TreeInt{}
  case class LeafI(i:Int) extends TreeInt
  case class NodeI(l:TreeInt,r:TreeInt) extends TreeInt


  implicit def regularTreeInt:Regular[TreeInt]=new Regular[TreeInt]{

    //type PF[Z] = K[Int,Z]:+:(I[Z]:*:I[Z])
    type PFL[Z] = K[Int,Z]
    type PFR[Z] = (I[Z]:*:I[Z])
    type PF[Z]=PFL[Z]:+:PFR[Z]

    val ff=implicitly[Functor[PF]]

    def from(t:TreeInt):PF[TreeInt] = t match {
      case LeafI(n) => L(K(n))
      case NodeI(x,y) => R(:*:(I(x),I(y)))
    }

    def to(pf:PF[TreeInt]):TreeInt=  pf match {
      case L(K(n)) => LeafI(n)
      case R(:*:(I(x),I(y))) => NodeI(x,y)
    }
  }

  implicit def regularList[A]:Regular[List[A]]=new Regular[List[A]]{
  	//type PF[Z] = U[Z]:+:(K[A,Z]:*:I[Z])
  	type PFL[Z] = U[Z]
  	type PFK[Z] = K[A,Z]
  	type PFR[Z] = PFK[Z]:*:I[Z]
  	type PF[Z] = PFL[Z]:+:PFR[Z]
  
  	val ff=implicitly[Functor[PF]]

	  def from(t:List[A]):PF[List[A]] = t match {
	    case Nil => L(U())
	    case x::xs => R(:*:(K(x),I(xs)))
	  }

	  def to(pf:PF[List[A]]):List[A]=  pf match {
	    case L(U()) => Nil
	    case R(:*:(K(x),I(xs))) => x::xs
	  }
  }

  //parametrized binary tree
  trait Tree[A]{}
  case class Leaf[A](i:A) extends Tree[A]
  case class Node[A](l:Tree[A],r:Tree[A]) extends Tree[A]


  implicit def regularTree[A]:Regular[Tree[A]]=new Regular[Tree[A]]{

    //type PF[Z] = K[A,Z]:+:(I[Z]:*:I[Z])
    type PFL[Z] = K[A,Z]
    type PFR[Z] = (I[Z]:*:I[Z])
    type PF[Z]=PFL[Z]:+:PFR[Z]

    val ff=implicitly[Functor[PF]]

    def from(t:Tree[A]):PF[Tree[A]] = t match {
      case Leaf(n) => L(K(n))
      case Node(x,y) => R(:*:(I(x),I(y)))
    }

    def to(pf:PF[Tree[A]]):Tree[A]=  pf match {
      case L(K(n)) => Leaf(n)
      case R(:*:(I(x),I(y))) => Node(x,y)
    }
  }

  // With Bifunctors

  implicit def regular2List:Regular2[List]=new Regular2[List]{
  	type PFL[A,Z] = U2[A,Z]
  	type PFR[A,Z] = Par[A,Z]:*:Rec[A,Z] 
  	type PF2[A,Z]  = PFL[A,Z]:+:PFR[A,Z] 
  
  	val bf=implicitly[Bifunctor[PF2]]

	  def from2[A](t:List[A]):PF2[A,List[A]] = t match {
	    case Nil => L(U2())
	    case x::xs => R(:*:(Par(x),Rec(xs)))
	  }

	  def to2[A](pf:PF2[A,List[A]]):List[A]=  pf match {
	    case L(U2()) => Nil
	    case R(:*:(Par(x),Rec(xs))) => x::xs
	  }
  }

  implicit def regular2Tree[A]:Regular2[Tree]=new Regular2[Tree]{

    //type PF[Z] = K[A,Z]:+:(I[Z]:*:I[Z])
    type PFL[A,Z] = Par[A,Z]
    type PFR[A,Z] = (Rec[A,Z]:*:Rec[A,Z])
    type PF2[A,Z]=PFL[A,Z]:+:PFR[A,Z]

    val bf=implicitly[Bifunctor[PF2]]

    def from2[A](t:Tree[A]):PF2[A,Tree[A]] = t match {
      case Leaf(n) => L(Par(n))
      case Node(x,y) => R(:*:(Rec(x),Rec(y)))
    }

    def to2[A](pf:PF2[A,Tree[A]]):Tree[A]=  pf match {
      case L(Par(n)) => Leaf(n)
      case R(:*:(Rec(x),Rec(y))) => Node(x,y)
    }
  }
  
  //rose tree
  case class Rose[A](a:A, childs:List[Rose[A]])

  implicit def regular2Rose[A]:Regular2[Rose]=new Regular2[Rose]{

    //type PF[Z] = Par[A,Z]:*:(List[A] :@@: Rec[A,Z]) 
    type PFL[A,Z] = Par[A,Z]
    type PFR[A,Z] = (List[A]:*:Rec[A,Z])
    type PF2[A,Z]=Comp2[PFL,PFR,A,Z]

    val bf=implicitly[Bifunctor[PF2]]

    def from2[A](t:Rose[A]):PF2[A,Rose[A]] = t match {
      case Rose(a,childs) => ???
    }

    def to2[A](pf:PF2[A,Tree[A]]):Tree[A]=  pf match {
      case L(Par(n)) => Leaf(n)
      case R(:*:(Rec(x),Rec(y))) => Node(x,y)
    }
  }
  

}
