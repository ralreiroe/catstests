package catstests

import cats.Monad
import org.scalatest.{FlatSpec, Matchers}

class OnTrackWithList2 extends FlatSpec with Matchers {

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


    class MyAlgUsingList extends MyAlg[List] {
      override def insertItSomewhere​(someInt: Int): List[Unit] = List(println(someInt))
      override def doSomething(someInt: Int): List[Int] = List(someInt+1)
    }

    class MyProgUsingList(myAlg: MyAlg[List]) extends MyProg[List](myAlg: MyAlg[List])

    val value1: MyProg[List] = new MyProgUsingList(new MyAlgUsingList)
    println(value1.checkThenAddIt(6))

  }




}
