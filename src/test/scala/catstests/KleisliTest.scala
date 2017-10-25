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

  "normal composition" should "work with andThen applied to functions" in {

    val make: (Int) => Make = (_) => Make(1, "Suzuki")

    val parts: Make => List[Part] = {
      case Make(1, _) => List(Part(1, "Gear Box"), Part(2, "Clutch cable"))
      case _ => Nil
    }

    val g: (Int) => List[Part] = make andThen parts

    g(1) shouldBe List(Part(1,"Gear Box"), Part(2,"Clutch cable"))


  }


  "composing a ```=> Option[T]``` with a ```T => Option[U]``` function" should "work with Kleisli(```T => ```)" in {

    val make: (Int) => Option[Make] = (x: Int) => x match {
      case 1 => Some(Make(1, "Suzuki"))
      case _ => None
    }

    val parts: (Make) => Option[NonEmptyList[Part]] = (x: Make) => x match {
      case Make(1, _) => Some(NonEmptyList.of(Part(1, "Gear Box"), Part(2, "Clutch cable")))
      case _ => None
    }

    val g: Kleisli[Option, Int, NonEmptyList[Part]] = Kleisli(parts).compose(make)
    val h: Kleisli[Option, Int, NonEmptyList[Part]] = Kleisli(make) andThen Kleisli(parts)

    val maybeList: Option[NonEmptyList[Part]] = g(1)
    println(maybeList)

    g(1) shouldBe Some(NonEmptyList(Part(1,"Gear Box"), List(Part(2,"Clutch cable"))))
    h(1) shouldBe Some(NonEmptyList(Part(1,"Gear Box"), List(Part(2,"Clutch cable"))))



    val parse: String => Option[Int] =
      s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

    val reciprocal: Int => Option[Double] =
      i => if (i != 0) Some(1.0 / i) else None

    val k = Kleisli(reciprocal).compose(parse)
    val m = Kleisli(parse) andThen Kleisli(reciprocal)

    k("5") shouldBe Some(0.2)
    k("lll") shouldBe None
    m("5") shouldBe Some(0.2)
    m("lll") shouldBe None
  }


  "composing two option-returning functions" should "work II" in  {

    def foo(n: Int): Int => Option[Int] = {x => if (x == n) none else x.some}

    val f0: (Int) => Option[Int] = foo(0)
    val f1 = foo(1)

    val composed = Kleisli(f0).andThen(Kleisli(f1))

    println(f0(0))
    println(f0(1))
    println(s"===${composed(2)}")
    println(s"===${composed(0)}")

    val composed2 = Kleisli(f1).andThen(f0)

  }


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
