package catstests

import cc.Spec

class OptionApplicative extends Spec {

  "list functor should work" in {


    import cats._
    import cats.data._
    import cats.implicits._

    println(
      Apply[Option].ap(
        {{(_: Int) + 3}.some }
      )(9.some)
    )

    val ff: Option[Int => Int] = {
      Some (
        (_: Int) + 3
      )
    }
    val ff2: Option[(Int) => Int] = Some(i => i + 3)
    println(
      Apply[Option].ap(
        ff
      )(9.some)
    )

    /**
      *
      * cats.Flatmap
      *
      *   override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)(f => map(fa)(f))
      */

    println(
//      Apply[Option].flatMap(9.some)(f => Apply[Option].map(Some(9))(f))
    )


    FlatMap[List].flatMap(List(1,2,3))(_ => List(4,5)) mustBe List(4, 5, 4, 5, 4, 5)
    List(1,2,3).flatMap(_ => List(4,5)) mustBe List(4, 5, 4, 5, 4, 5)

  }

}
