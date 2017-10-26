package catstests

import cats.data.Kleisli
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class KleisliTestTry extends FlatSpec with Matchers {

  "Composition of two function A -> Try[B]" should "work via flatmap or Kleisli" in {

    val firstTwo: (List[Int]) => Try[List[Int]] = (l: List[Int]) => Try(List(l.head, l.tail.head))

    val reverse: (List[Int]) => Try[List[Int]] = (l: List[Int]) => Success(l.reverse)

    val firsttwoReversed: (List[Int]) => Try[List[Int]] = (ints: List[Int]) => for {
      r1 <- firstTwo(ints)
      r2 <- reverse(r1)             //reverse MUST return a Try
    } yield r2

    firsttwoReversed(List(1, 2, 3)) shouldBe Success(List(2,1))
    firsttwoReversed(List(1)) shouldBe a[Failure[_]]

    val firsttwoReversedViaKleisli = Kleisli(firstTwo) andThen Kleisli(reverse)

    firsttwoReversedViaKleisli(List(1, 2, 3)) shouldBe Success(List(2,1))
    firsttwoReversedViaKleisli(List(1)) shouldBe a[Failure[_]]


  }
}
