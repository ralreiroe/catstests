package catstests

import cats.Monad
import org.scalatest.{FlatSpec, Matchers}

class OnTrack extends FlatSpec with Matchers {

  trait MyAlg[F[_]] {
    def insertItSomewhere​(someInt: Int): F[Unit]
    def doSomething(someInt: Int): F[Int]
  }

  "" should "" in {

    import cats.implicits._

    abstract class MyProg[F[_]](myAlg: MyAlg[F]) {

      def checkThenAddIt(someInt: Int)(implicit F: Monad[F]) = {
        val intsInF = myAlg.doSomething(someInt)
        F.flatMap(intsInF) {a => myAlg.insertItSomewhere​(a)}
      }
    }


    val myalg2 = new MyAlg[Option] {
      override def insertItSomewhere​(someInt: Int): Option[Unit] = Some(println(someInt))

      override def doSomething(someInt: Int): Option[Int] = Some(someInt+1)
    }


    val value1: MyProg[Option] = new MyProg[Option](myalg2) {}
    println(value1.checkThenAddIt(6))

  }




}
