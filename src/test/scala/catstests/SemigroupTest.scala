package catstests

import cats.implicits._
import cc.Spec


/**
  *
  * https://www.scala-exercises.org/cats/semigroup
  * https://stackoverflow.com/questions/1262741/scala-how-to-merge-a-collection-of-maps
  */
class SemigroupTest extends Spec {

  "semigroup-combining should work" in {

    import cats.kernel.Semigroup

    Semigroup[Int].combine(1, 2) mustBe (3)
    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) mustBe List(1, 2, 3, 4, 5, 6)
    Semigroup[Option[Int]].combine(Option(1), Option(2)) mustBe Some(3
    )
    Semigroup[Option[Int]].combine(Option(1), None) mustBe Some(1
    )
    Semigroup[Int ⇒ Int]
      .combine({ (x: Int) ⇒
        x + 1
      }, { (x: Int) ⇒
        x * 10
      })
      .apply(6) mustBe 67
  }

  "semigroup combining should work using combine / |@|" in {

    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None

    one |+| two mustBe Some(3)
    n |+| two mustBe Some(2)
    n |+| n mustBe None

  }


  """Many of these types have methods defined directly on them, which allow for such combining, e.g. ++ on List, but the value of having a Semigroup type class available is that these compose, so for instance, we can do
    |a combine on Map which is a member of the following typeclass:
    |
    |
    |class MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]]  { //A monoid is a semigroup with an identity: trait Monoid[@sp(Int, Long, Float, Double) A] extends Any with Semigroup[A] {
    |
    |  def empty: Map[K, V] = Map.empty
    |
    |  def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
    |    if (xs.size <= ys.size) {
    |      xs.foldLeft(ys) { case (my, (k, x)) =>
    |        my.updated(k, Semigroup.maybeCombine(x, my.get(k)))      //uses semigroup combine on the values
    |      }
    |    } else {
    |      ys.foldLeft(xs) { case (mx, (k, y)) =>
    |        mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
    |      }
    |    }
    |
    |
    |    ==> see cats.kernel.Monoid / cats.kernel MapMonoid
  """.stripMargin in {

    import cats.kernel.Semigroup

    Map("foo" -> List(1, 2)) ++ Map("foo" -> List(3, 4), "bar" -> List(42)) mustBe Map("foo" -> List(3, 4), "bar" -> List(42))    //normal combine on Map replaces values

    //Semigroup combining on Map combines values instead

    val aMap = Map("foo" → Map("bar" → 5))
    val anotherMap = Map("foo" → Map("bar" → 6))

    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
    combinedMap.get("foo") mustBe Some(Map("bar" -> 11))

    /**
      *   implicit def catsKernelStdMonoidForMap[K, V: Semigroup]: Monoid[Map[K, V]] =
    new MapMonoid[K, V]

      */


    Map("foo" -> List(1, 2)).combine(Map("foo" -> List(3, 4), "bar" -> List(42))) mustBe  Map("foo" -> List(1, 2, 3, 4), "bar" -> List(42))


  }

}
