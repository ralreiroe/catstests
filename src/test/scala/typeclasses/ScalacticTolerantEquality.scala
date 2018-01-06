package typeclasses

import org.scalatest.{FlatSpec, Matchers}

/**
  * http://www.scalactic.org/user_guide/CustomEquality
  */
class ScalacticTolerantEquality extends FlatSpec with Matchers {

  "tolerant equality " should "work" in {

    1.01 === 1.00 shouldBe false

  }

  "tolerant equality " should "work II" in {

    val tolerance = 0.01

    implicit val doubleEquality = new org.scalactic.Equality[Double] {
      def areEqual(a: Double, b: Any): Boolean = {
        b match {
          case bDouble: Double => (a <= bDouble + tolerance) && (a >= bDouble - tolerance)
          case _ => false
        }
      }
    }

    1.01 === 1.00 shouldBe true

  }


}
