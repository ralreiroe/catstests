package catstests

import cats.Monad
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{ExecutionContext, Future}

class OnTrackWithFuture extends FlatSpec with Matchers {

  trait MyAlg[F[_]] {
    def insertItSomewhere​(someInt: Int): F[Unit]
    def doSomething(someInt: Int): F[Int]
  }

  "" should "" in {

    import cats.implicits._

    abstract class MyProg[F[_]](myAlg: MyAlg[F]) {

      def checkThenAddIt(someInt: Int)(implicit F: Monad[F]) =
        F.flatMap(myAlg.doSomething(someInt)) {a => myAlg.insertItSomewhere​(a)}
    }


    class MyAlgUsingFuture(implicit ec: ExecutionContext) extends MyAlg[Future] {
      override def insertItSomewhere​(someInt: Int): Future[Unit] = Future(println(someInt))
      override def doSomething(someInt: Int): Future[Int] = Future(someInt+1)
    }

    class MyProgUsingFuture(myAlg: MyAlg[Future]) extends MyProg[Future](myAlg: MyAlg[Future])

    import scala.concurrent.ExecutionContext.Implicits.global

    val value1: MyProg[Future] = new MyProgUsingFuture(new MyAlgUsingFuture)
    println(value1.checkThenAddIt(6))

  }




}
