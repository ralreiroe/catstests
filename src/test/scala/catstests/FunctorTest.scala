package catstests

import cats.Functor
import cats.implicits._
import cats.kernel.Semigroup
import cc.Spec

class FunctorTest extends Spec {

  "list functor should work" in {

    val a = Functor[List].map(List(1, 2, 3, 4))(x => x + 4)
    println(a)

    case class Box[T](val item: T)
    implicit val boxFunctor = new Functor[Box] {
      override def map[A, B](fa: Box[A])(f: (A) => B): Box[B] = new Box(f(fa.item))
    }

    val b = Functor[Box].map(new Box(40))(_ * 2)
    println(b)

  }

}
