package typeclasses

import cc.Spec

class FunctorTypeclass extends Spec {

  "list functor should work" in {

    import cats.Functor
    import cats.implicits._

    val a = Functor[List].map(List(1, 2, 3, 4))(x => x + 4)
    println(a)

    case class Box[T](val item: T)

    implicit val boxFunctor = new Functor[Box] {
      override def map[A, B](fa: Box[A])(f: (A) => B): Box[B] = new Box(f(fa.item))
    }

    Functor[Box].map(new Box(40))(_ * 2) mustBe Box(80)

  }

  "" in {

    trait Functor[T[_]] {
      def map[A, B](fa: T[A])(f: A => B): T[B]
    }

    case class Box[T](val item: T)

    implicit val boxFunctor = new Functor[Box] {
      override def map[A, B](fa: Box[A])(f: A => B): Box[B] = new Box(f(fa.item))
    }

    def map[F[_], A,B](fa: F[A])(f: A => B)(implicit functor: Functor[F]) = functor.map(fa)(f)          //<=========same as what is defined in cats.Functor[F[_}}

//    object Functor {
//      def apply[F[_]](implicit instance : Functor[F]) = instance.map(fa)(f)
//    }

    map(new Box(40))(_ * 2) mustBe Box(80)


  }

}
