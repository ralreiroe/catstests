package catstests

import cats.Functor
import cats.data.{Kleisli, NonEmptyList}
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}


/**
  * http://typelevel.org/cats/datatypes/kleisli.html
  *
  * http://www.leonardoborges.com/writings/2014/06/17/functional-composition-with-monads-kleisli-functors/
  *
  * https://www.codementor.io/ysusuk/aws-s3-api-using-scala-du1085b9f
  *
  *
  */
class KleisliTestDifferentMonads extends FlatSpec with Matchers {

    case class Make(id: Int, name: String)
    case class Part(id: Int, name: String)

  "composing two function returning different monads" should "work using functor lifting" in  {

    val make: (Int) => Option[Make] = (x: Int) => x match {
      case 1 => Some(Make(1, "Suzuki"))
      case _ => None
    }

    val parts: (Make) => List[Part] = (x: Make) => x match {
      case Make(1, _) => List(Part(1, "Gear Box"), Part(2, "Clutch cable"))
      case _ => Nil
    }

    val liftparts: (Option[Make]) => Option[List[Part]] = Functor[Option].lift(parts)
    val g: (Int) => Option[List[Part]] = liftparts.compose(make)

    println(g(1))
    println(g(2))

    val h: (Int) => Option[List[Part]] = make(_).map(parts)



  }
}
