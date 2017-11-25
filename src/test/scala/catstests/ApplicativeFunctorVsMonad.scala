package catstests

import cats.Apply
import cats.syntax.CartesianBuilder
import cc.Spec

/**
  * Note that Validation[Whatever, _] isn't a monad (for reasons discussed here, for example), but ValidationNel[String, _] is an applicative functor
  *
  *
  * ap would accumulate errors whereas (pseudo-)monadic composition would only operate on the value part of Validation.

Therefore, one cannot be expressed in terms of the other and thus no monad instance exists for Validation.
  *
  */
class ApplicativeFunctorVsMonad extends Spec {

  "list functor should work" in {


    //https://stackoverflow.com/questions/12307965/method-parameters-validation-in-scala-with-for-comprehension-and-monads/12309023#12309023


    def incremented(i: Int): Int = i + 1

    val x = Some(1)

    //option is a functor, ie. has a map method, hence
    val incrementedOnOption: (Option[Int] => Option[Int]) = x => x map incremented

    //We've "lifted" incremented into the Option functor; that is, we've essentially changed a function mapping Int to Int into one mapping Option[Int] to Option[Int]


    def add(i: Int, j: Int): Int = i + j

    case class User(name: String, age: Int)


    def addOnOptionsAsMonad(uo: Option[Int], uo2: Option[Int]) = uo.flatMap(u => uo2.map(u2 => add(u,u2)))

    import cats.implicits._
    def addOnOptionsAsApplicative(uo: Option[Int], uo2: Option[Int]): Option[Int] = (uo |@| uo2).map((a, b) => add(a,b))


    val users = List(User("John", 38), User("Mary", 44))

    val u1: Option[Int] = users.find(_.name == "John").map(_.age)
    val u2: Option[Int] = users.find(_.name == "Mary").map(_.age)

    println(addOnOptionsAsMonad(u1, u2))
    println(addOnOptionsAsApplicative(u1, u2))


    val ff: Option[Int => Int => Int] = Some(i=> j => add(i,j))

    val fff: (Option[Int]) => Option[(Int) => Int] = Apply[Option].ap(
      ff
    )

    val ffff: Option[(Int) => Int] = fff(Some(1))

    val fffff = fff(Some(1))

//    ((Some(1), Some(2)))

    //https://stackoverflow.com/questions/21351391/how-to-accumulate-errors-in-either


    /*for-comprehensions (which desugar to a combination of calls to flatMap and map) are designed to allow you to sequence monadic computations in such a way that you have access to the result of earlier computations in subsequent steps. Consider the following:*/

    def parseInt(s: String) = try Right(s.toInt) catch {
      case _: Throwable => Left("Not an integer!")
    }

    def checkNonzero(i: Int) = if (i == 0) Left("Zero!") else Right(i)

    def inverse(s: String): Either[String, Double] = for {
      i <- parseInt(s).right
      v <- checkNonzero(i).right
    } yield 1.0 / v

  }

}
