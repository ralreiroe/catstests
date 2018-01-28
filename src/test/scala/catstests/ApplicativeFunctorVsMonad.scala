package catstests

import cats.Apply
import cats.syntax.CartesianBuilder
import cc.Spec

/**
  *
  * https://stackoverflow.com/questions/19880207/when-and-why-should-one-use-applicative-functors-in-scala
  *
  * Writing applicative code allows you to avoid making unnecessary claims about ***dependencies between computations—claims that similar monadic code would commit you to. A sufficiently smart library or compiler could in principle take advantage of this fact.

To make this idea a little more concrete, consider the following monadic code:

case class Foo(s: Symbol, n: Int)

val maybeFoo = for {
  s <- maybeComputeS(whatever)
  n <- maybeComputeN(whatever)
} yield Foo(s, n)
The for-comprehension desugars to something more or less like the following:

val maybeFoo = maybeComputeS(whatever).flatMap(
  s => maybeComputeN(whatever).map(n => Foo(s, n))
)
We know that maybeComputeN(whatever) does ***not depend on s (assuming these are well-behaved methods that aren't changing some mutable state behind the scenes), but the compiler doesn't—from its perspective it needs to know s before it can start computing n.

The applicative version (using Scalaz) looks like this:

val maybeFoo = (maybeComputeS(whatever) |@| maybeComputeN(whatever))(Foo(_, _))
Here we're ***explicitly stating that there's no dependency between the two computations
  *
  *
  *
  * Note that Validation[Whatever, _] isn't a monad (for reasons discussed here, for example), but ValidationNel[String, _] is an applicative functor
  *
  *
  * ap would accumulate errors whereas (pseudo-)monadic composition would only operate on the value part of Validation.

Therefore, one cannot be expressed in terms of the other and thus no monad instance exists for Validation.
  *
  */
class ApplicativeFunctorVsMonad extends Spec {


  "monadic composition make sense in most situations. " in {

    /*for-comprehensions (which desugar to a combination of calls to flatMap and map) are designed to allow you to sequence monadic computations in such a way that you have access to the result of earlier computations in subsequent steps. Consider the following:*/

    def parseInt(s: String) = try Right(s.toInt) catch {
      case _: Throwable => Left("Not an integer!")
    }

    def checkNonzero(i: Int) = if (i == 0) Left("Zero!") else Right(i)

    def inverse(s: String): Either[String, Double] = for {
      i <- parseInt(s).right
      v <- checkNonzero(i).right    //calling this only makes sense if parseInt is a right
    } yield 1.0 / v


  }

  "Options are monads as well as applicative functors" in {


    //https://stackoverflow.com/questions/12307965/method-parameters-validation-in-scala-with-for-comprehension-and-monads/12309023#12309023


    def incremented(i: Int): Int = i + 1

    val x = Some(1)

    //option is a functor, ie. has a map method, hence
    val incrementedOnOption: (Option[Int] => Option[Int]) = x => x map incremented

    //We've "lifted" incremented into the Option functor; that is, we've essentially changed a function mapping Int to Int into one mapping Option[Int] to Option[Int]


    def add(i: Int, j: Int): Int = i + j

    case class User(name: String, age: Int)


    def addOnOptionsAsMonad(uo: Option[Int], uo2: Option[Int]) = uo.flatMap(u => uo2.map(u2 => add(u, u2)))

    import cats.implicits._
    def addOnOptionsAsApplicative(uo: Option[Int], uo2: Option[Int]): Option[Int] = (uo |@| uo2).map((a, b) => add(a, b))


    val users = List(User("John", 38), User("Mary", 44))

    val u1: Option[Int] = users.find(_.name == "John").map(_.age)
    val u2: Option[Int] = users.find(_.name == "Mary").map(_.age)

    When("composing two options")
    println(addOnOptionsAsMonad(u1, u2))
    println(addOnOptionsAsApplicative(u1, u2))

    Then("it seems it does not matter whether as applicative functors or monads")
    val un: Option[Int] = None
    println(addOnOptionsAsMonad(un, u2))
    println(addOnOptionsAsApplicative(un, u2))


    val ff: Option[Int => Int => Int] = Some(i => j => add(i, j))

    val fff: (Option[Int]) => Option[(Int) => Int] = Apply[Option].ap(
      ff
    )

    val ffff: Option[(Int) => Int] = fff(Some(1))

    val fffff = fff(Some(1))

    //    ((Some(1), Some(2)))

    //https://stackoverflow.com/questions/21351391/how-to-accumulate-errors-in-either


  }


}
