package shapeless

import cc.Spec

class HListIntro extends Spec {

  "a standard list" in {

    sealed abstract class List[+T] {
      def ::[U >: T](head: U): List[U] = new ::[U](head, this)
    }
    case class ::[T](head: T, tail: List[T]) extends List[T]
    case object Nil extends List[Nothing]

    def sumInts(l: List[Int]): Int = l match {
      case Nil => 0 // termination case
      case head :: tail => head + sumInts(tail) // recursion
    }

    sumInts(1 :: 2 :: 3 :: Nil) mustBe 6 //a :: b  is desugared to b.::(a)

  }

  "an HList is a mix between a tuple and a list; a extendible tuple" in {

//    import shapeless._

//
//    // Simplified definition:
//    sealed trait HList extends Product with Serializable
//    final case class ::[+H, +T <: HList](head : H, tail : T) extends HList
//    sealed trait HNil extends HList {
//      def ::[H](h : H) = shapeless.::(h, this)
//    }
//    case object HNil extends HNil

    val l: ::[Int, ::[String, ::[Boolean, HNil]]] = 1 :: "hello" :: true :: HNil

    println(l)

    l.drop(3)

    """(1 :: "hello" :: true :: HNil).drop(4)""" mustNot compile




  }


}
