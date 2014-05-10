package reggen

import Functors._

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
}