package typeclasses.vanilla


import org.scalatest._

class TypeClassTest4 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    //a class that implements the compare method in its own type hierarchy
    case class ComparableValue(i: Int) {
      def compareTo(that: ComparableValue) = this.i - that.i
    }

    val comparablevals = List(ComparableValue(1), ComparableValue(1), ComparableValue(2), ComparableValue(2),
      ComparableValue(3))

    //use of compareTo by simply calling the method on ComparableValue
    def calcMin(listOfComparables: List[ComparableValue]) =
                listOfComparables.reduceLeft((res, curr) => if (res.compareTo(curr) < 0) res else curr)

    println(calcMin(comparablevals))

    //now in contrast...

    //a class that does *not* implement a compare method in its type hierarchy anywhere
    case class NonComparableValue(value: Int)

    //so we define a compare method in a separate class
    class ValueComparator {
      def compareTo(x: NonComparableValue, y: NonComparableValue): Int = x.value-y.value
    }

    //now, to calculate the min, we need an instance of our separate comparator class
    def calcMin2(listOfNonComparables: List[NonComparableValue], vc: ValueComparator) =
      listOfNonComparables.reduceLeft((res, curr) => if (vc.compareTo(res, curr) < 0) res else curr)  //compare this: vc.compareTo(res, curr) to res.compareTo(curr) above
                                      //the operation compareTo is not on our NonComparableValue but on our ValueComparator which takes two arguments not one
                                      //so...
                                      // (a) we are delegating to another class
                                      // (b) and this other class is operating directly on our NonComparableValue

    val comparator = new ValueComparator

    val nonComparableValues = List(NonComparableValue(1), NonComparableValue(2), NonComparableValue(3), NonComparableValue(2), NonComparableValue(1))

    println(calcMin2(nonComparableValues, comparator))



    //now in contrast to the delegation model we use an adaptor model

    //the adaptor class defines the operation
    case class ComparableAdaptor(adaptee: NonComparableValue) {
      def compareTo(that: ComparableAdaptor) = this.adaptee.value - that.adaptee.value
    }

    //to calculate the min is a method defined on a list of adaptors rather than the original instances
    def calcMin3(listOfComparables: List[ComparableAdaptor]) =
    listOfComparables.reduceLeft((res, curr) => if (res.compareTo(curr)<0) res else curr )

    //in the adaptor model we need to wrap every instance to make use of the new operation
    val list: List[ComparableAdaptor] = nonComparableValues.map(new ComparableAdaptor(_))

    println(calcMin3(list))
  }
}




