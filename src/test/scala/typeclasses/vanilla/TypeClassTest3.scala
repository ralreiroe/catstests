package typeclasses.vanilla


import java.util.Comparator

import org.scalatest._

class TypeClassTest3 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    //a class that implements the compare method in its own type hierarchy
    case class ComparableValue(i: Int) extends java.lang.Comparable[ComparableValue] {
      def compareTo(that: ComparableValue) = this.i - that.i
    }

    val comparablevals = List(ComparableValue(1), ComparableValue(1), ComparableValue(2), ComparableValue(2),
      ComparableValue(3))

    //usage by simply calling the method on ComparableValue
    comparablevals.reduceLeft((res, curr) => if (res.compareTo(curr) < 0) res else curr)


    //a class that does not implement a compare method in its type hierarchy anywhere
    case class NonComparableValue(value: Int)

    //so we define a compare method in a separate class
    class ValueComparator extends Comparator[NonComparableValue] {
      def compare(x: NonComparableValue, y: NonComparableValue): Int = x.value-y.value
    }

    val comparator = new ValueComparator

    val nonComparableValues = List(NonComparableValue(1), NonComparableValue(2), NonComparableValue(3), NonComparableValue(2), NonComparableValue(1))

    val arr = new Array[NonComparableValue](nonComparableValues.length)
    var i = 0
    for (x <- nonComparableValues) {
      arr(i) = x
      i += 1
    }

    println(arr.toList)
    java.util.Arrays.sort(arr, comparator)
    println(arr.toList)


    nonComparableValues.reduceLeft((res, curr) => if (comparator.compare(res, curr) <0) res else curr)

  }
}




