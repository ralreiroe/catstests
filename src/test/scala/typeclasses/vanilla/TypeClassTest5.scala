package typeclasses.vanilla


import org.scalatest._

class TypeClassTest5 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    //a class that implements the compare method in its own type hierarchy
    case class NumericValue(i: Int) {
      def plus(that: NumericValue) = this.i - that.i
    }

    val comparablevals = List(NumericValue(1), NumericValue(1), NumericValue(2), NumericValue(2),
      NumericValue(3))

    //use of compareTo by simply calling the method on ComparableValue
    def calcSum(listOfComparables: List[NumericValue]) =
                listOfComparables.reduceLeft((res, curr) => if (res.plus(curr) < 0) res else curr)

    println(calcSum(comparablevals))

    //now in contrast...

    //a class that does *not* implement a compare method in its type hierarchy anywhere
    case class NonNumericValue(value: Int)

    //so we define a compare method in a separate class

    trait NumberLike[T] {
      def plus(x: T, y: T): T
    }
    object NumberLikeNonNumericValue extends NumberLike[NonNumericValue] {
      def plus(x: NonNumericValue, y: NonNumericValue): NonNumericValue =
        new NonNumericValue(x.value + y.value)
    }


      //now, to calculate the min, we need an instance of our separate comparator class
    def calcMin2(listOfNonComparables: List[NonNumericValue], vc: NumberLike[NonNumericValue]) =
      listOfNonComparables.reduce(vc.plus(_, _))


    val nonComparableValues = List(NonNumericValue(1), NonNumericValue(2), NonNumericValue(3), NonNumericValue(2), NonNumericValue(1))

    println(calcMin2(nonComparableValues, NumberLikeNonNumericValue))

  }
}




