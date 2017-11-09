package catstests


import cats.implicits._
import cc.Spec

import scala.util.Try

class TraverseTest extends Spec {

  "traverse should work" in {

    def parseToInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption

    val res1: List[Option[Int]] = List("1", "2", "3").map(parseToInt)
    println(res1)
    res1 mustBe List(Some(1), Some(2), Some(3))

    val res: Option[List[Int]] = List("1", "2", "3").traverse(parseToInt)
    println(res)
    res mustBe Some(List(1, 2, 3))

    val x: List[String] = List("1", "two", "3")
    println(x)

    val x0: List[Option[Int]] = x.map(parseToInt)
    println(x0)

    val x1: Option[List[Int]] = x.traverse(parseToInt)
    println(x1)

    x1 mustBe None

//    val x2: List[Option[Int]] = x.flatTraverse(_.map(parseToInt))
//
//    x2 mustBe List(Some(1), None, Some(3))

  }

  "traverse on try" in {
    def parseToInt(s: String): Try[Int] = Either.catchOnly[NumberFormatException](s.toInt).toTry

    val res1 =  List("1", "2", "3").map(parseToInt)
    println(res1)

    val res =  List("1", "2", "3").traverse(parseToInt)
    println(res)

    val x =  List("1", "two", "3")
    println(x)

    val x0 =  x.map(parseToInt)
    println(x0)

    val x1 =  x.traverse(parseToInt)
    println(x1)


  }



}
