package catstests

import cats.implicits._
import cats.kernel.Semigroup
import cc.Spec
import org.scalatest.Matchers

class SemigroupTest extends Spec with Matchers {

  "semigroup-combining should work" in {

    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None

    one |+| two mustBe Some(3)
    n |+| two mustBe Some(2)
    n |+| n mustBe None

    val aMap = Map("foo" → Map("bar" → 5))
    val anotherMap = Map("foo" → Map("bar" → 6))

    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

    combinedMap.get("foo") shouldBe Some(Map("bar" -> 11))


    println(Map("foo" -> List(1, 2)) ++ Map("foo" -> List(3, 4), "bar" -> List(42)))

    println(Map("foo" -> List(1, 2)).combine(Map("foo" -> List(3, 4), "bar" -> List(42))))

  }

}
