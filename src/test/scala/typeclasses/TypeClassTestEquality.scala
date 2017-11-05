package typeclasses

import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * https://stackoverflow.com/questions/5408861/what-are-type-classes-in-scala-useful-for
  *
  *
  * Typeclasses can very easily represent notions that are quite difficult to represent in the presence of subtyping, such as equality and ordering.
Exercise: create a small class/trait hierarchy and try to implement .equals on each class/trait in such a way that the operation over arbitrary instances from the hierarchy is properly reflexive, symmetric, and transitive.


      type Color = Int
    class Point(x : Int, y : Int)
    class ColoredPoint( x : Int, y : Int, c : Color) extends Point(x, y)


    //So according to the definition the ColorPoint(1,4,red) should be equal to the Point(1,4) they are the same Point after all. So ColorPoint(1,4,blue) should also be equal to Point(1,4), right? But of course ColorPoint(1,4,red) should not equal ColorPoint(1,4,blue), because they have different colors. There you go, one basic property of the equality relation is broken.

  */
trait Equality[T] {                           //<====== the "type class"
  def areEqual(a: T, b: Any): Boolean
}

class TypeClassTestEquality extends FlatSpec with Matchers {
  "equality " should "work" in {

    implicit val doubleEquality = new Equality[Double] {
      def areEqual(a: Double, b: Any): Boolean = {
        b == a                                          //<========a type class instance with implementation
      }
    }

    def isEqual[T](a: T, b: Any)(implicit equality: Equality[T]) = equality.areEqual(a, b)

    isEqual(1.00, 1.0) shouldBe true
    isEqual(1.01, 1.0) shouldBe false
  }

  "tolerant equality " should "work" in {

    val tolerance = 0.01

    implicit val doubleEquality = new Equality[Double] {    //<========a type class instance with another implementation
      def areEqual(a: Double, b: Any): Boolean = {
        b match {
          case bDouble: Double => (a <= bDouble + tolerance) && (a >= bDouble - tolerance)
          case _ => false
        }
      }
    }

    def isEqual[T](a: T, b: T)(implicit equality: Equality[T]) = equality.areEqual(a, b)

    isEqual(1.01, 1.0) shouldBe true

  }
  "tolerant equality " should "work with sugar" in {

    val tolerance = 0.01

    implicit val doubleEquality = new Equality[Double] {
      def areEqual(a: Double, b: Any): Boolean = {
        b match {
          case bDouble: Double => (a <= bDouble + tolerance) && (a >= bDouble - tolerance)
          case _ => false
        }
      }
    }

    def isEqual[T](a: T, b: T)(implicit equality: Equality[T]) = equality.areEqual(a, b)

    class Equalizer[T](ls: T) {

      def ====(rs: T)(implicit equality: Equality[T]) = {
        equality.areEqual(ls, rs)
      }
    }

    implicit def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)


    isEqual(1.01, 1.0) shouldBe true
    new Equalizer(1.01) ==== 1.00 shouldBe true
    1.01 ==== 1.00 shouldBe true


  }
}
