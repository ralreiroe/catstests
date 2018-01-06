package typeclasses.vanilla


import java.util.Comparator

import org.scalatest._

class TypeClassTest2 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    case class Value(i: Int) extends java.lang.Comparable[Value] {
      def compareTo(that: Value) = this.i - that.i
    }

    //    println(List(Value(1), Value(1), Value(2), Value(2), Value(3)).min)


    println(List(Value(1), Value(1), Value(2), Value(2), Value(3)).reduceLeft((x, y) => if (x.compareTo(y) < 0) x else y))

    val list1 = List(Value(1), Value(1), Value(2), Value(2), Value(3))

    val arr1 = new Array[Object](list1.length)
    var j = 0
    for (x <- list1) {
      arr1(j) = x
      j += 1
    }
    java.util.Arrays.sort(arr1)


    trait Ordering[T] extends Comparator[T] {
      def compare(x: T, y: T): Int
    }

    trait IntOrdering extends Ordering[Int] {
      def compare(x: Int, y: Int) =
        if (x < y) -1
        else if (x == y) 0
        else 1
    }
    implicit object IntO extends IntOrdering


    trait Box[T] {
      def value: T
    }
    case class IntBox(value: Int) extends Box[Int]

    class BoxOrdering[T](ordering: Ordering[T]) extends Ordering[Box[T]] {
      def compare(x: Box[T], y: Box[T]): Int = ordering.compare(x.value, y.value)
    }

    val ordering = new BoxOrdering(IntO)

    val list = List(IntBox(1), IntBox(2), IntBox(3), IntBox(2), IntBox(1))

    val arr = new Array[Box[Int]](list.length)
    var i = 0
    for (x <- list) {
      arr(i) = x
      i += 1
    }

    println(arr.toList)
    java.util.Arrays.sort(arr, ordering)
    println(arr.toList)


  }
}




