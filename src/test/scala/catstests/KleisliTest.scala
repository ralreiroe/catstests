package catstests

import cats.data.{Kleisli, NonEmptyList}
import org.scalatest.{FlatSpec, Matchers}
import cats.{FlatMap, Functor}
import cats.implicits._


/**
  * http://typelevel.org/cats/datatypes/kleisli.html
  *
  * http://www.leonardoborges.com/writings/2014/06/17/functional-composition-with-monads-kleisli-functors/
  *
  * https://www.codementor.io/ysusuk/aws-s3-api-using-scala-du1085b9f
  *
  *
  */
class KleisliTest extends FlatSpec with Matchers {

    case class Make(id: Int, name: String)
    case class Part(id: Int, name: String)

  "a" should "" in {

    val make: (Int) => Make = (_) => Make(1, "Suzuki")

    val parts: Make => List[Part] = {
      case Make(1, _) => List(Part(1, "Gear Box"), Part(2, "Clutch cable"))
    }

    val g = make andThen parts

    println(g(1))

  }


  "b" should "" in {

    val make: (Int) => Option[Make] = (x: Int) => x match {
      case 1 => Some(Make(1, "Suzuki"))
      case _ => None
    }

    val parts: (Make) => Option[NonEmptyList[Part]] = (x: Make) => x match {
      case Make(1, _) => Some(NonEmptyList.of(Part(1, "Gear Box"), Part(2, "Clutch cable")))
      case _ => None
    }

    val g: Kleisli[Option, Int, NonEmptyList[Part]] = Kleisli(parts).compose(make)

    println(g(1))



    val parse: String => Option[Int] =
      s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

    val reciprocal: Int => Option[Double] =
      i => if (i != 0) Some(1.0 / i) else None


    val h = Kleisli(reciprocal).compose(parse)

    println(h("5"))
    println(h("lll"))

  }

  "c" should "" in  {

    val make: (Int) => Option[Make] = (x: Int) => x match {
      case 1 => Some(Make(1, "Suzuki"))
      case _ => None
    }

    val parts: (Make) => List[Part] = (x: Make) => x match {
      case Make(1, _) => List(Part(1, "Gear Box"), Part(2, "Clutch cable"))
      case _ => Nil
    }

    val g: (Int) => Option[List[Part]] = Functor[Option].lift(parts).compose(make)

    println(g(1))
    println(g(2))

    val h: (Int) => Option[List[Part]] = make(_).map(parts)



  }

}
