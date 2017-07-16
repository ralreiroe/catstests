package catstests


import cats.implicits._
import cc.Spec

class TraverseTest extends Spec {

  "traverse should work" in {

    def parseToInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption

    val res1: List[Option[Int]] = List("1", "2", "3").map(parseToInt)

    val res: Option[List[Int]] = List("1", "2", "3").traverse(parseToInt)

    val x: Option[List[String]] = Option(List("1", "two", "3"))

    val x0: Option[List[Option[Int]]] = x.map {
      case (los) => los.map(parseToInt)
    }

    val x1: List[Option[Option[Int]]] = x.traverse {
      case (los) => los.map(parseToInt)
    }

    val x2: List[Option[Int]] = x.flatTraverse(_.map(parseToInt))

    x2 mustBe List(Some(1), None, Some(3))

  }

}
