package catstests


import cats.implicits._
import cc.Spec

import scala.util.{Failure, Success, Try}

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

    List("1", "2", "3").map(parseToInt) mustBe List(Success(1), Success(2), Success(3))

    List("1", "2", "3").traverse(parseToInt) mustBe Success(List(1, 2, 3))                                             //<=============

    val listCausingOneFailure = List("1", "two", "3")
    val x0: List[Try[Int]] = listCausingOneFailure.map(parseToInt)
    println(x0)
//    x0 mustBe List(Success(1), Failure(java.lang.NumberFormatException: For input string: "two"), Success(3))

    val x1: Try[List[Int]] = listCausingOneFailure.traverse(parseToInt)
    println(x1)
    x1.failed mustBe a[Success[_]]
    x1.failed.get mustBe a[NumberFormatException]
    x1.failed.get.getLocalizedMessage mustBe ("""For input string: "two"""")     //

    val x2: Try[List[Int]] = List("1", "two", "xxx").traverse(parseToInt)
    println(x2)



    //    x1 mustBe Failure(java.lang.NumberFormatException: For input string: "two")   //<=============


  }



}
