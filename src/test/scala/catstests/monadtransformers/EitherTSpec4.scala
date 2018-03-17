package catstests.monadtransformers

import cats.data.EitherT
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}


//http://eed3si9n.com/herding-cats/stacking-future-and-either.html


/**
  * also see OptionT
  * https://stackoverflow.com/questions/20693666/chaining-scala-try-instances-that-contain-options
  */
class EitherTSpec4 extends FlatSpec with Matchers with ScalaFutures {


  "EitherT" should "x" in {

    case class User(id: Long, name: String)

    // In actual code, probably more than 2 errors
    sealed trait Error
    object Error {

      final case class UserNotFound(userId: Long) extends Error

      final case class ConnectionError(message: String) extends Error

    }
    object UserRepo2 {
      def followersF(userId: Long): Future[Either[Error, List[User]]] = {
        userId match {
          case 0L => Future.successful(Right(List(User(1, "Michael"))))
          case 1L => Future.successful(Right(List(User(0, "Vito"))))
          case x => Future.successful(Left(Error.UserNotFound(x)))
        }
      }
    }

    import UserRepo2.followersF

    def isFriends1(user1: Long, user2: Long): Future[Either[Error, Boolean]] = {

      val res: Future[Either[Error, Boolean]] = for {
        (eitherFollowersOfUser1: Either[Error, List[User]]) <- followersF(user1) //pick out Either from the Future
        eitherFollowersOfUser2 <- followersF(user2)
      } yield for {
        followersOfUser1 <- eitherFollowersOfUser1 //pick out Right from the Either
        followersOfUser2 <- eitherFollowersOfUser2
      } yield followersOfUser1.exists(_.id == user2) && followersOfUser2.exists(_.id == user1)

      res
    }

    def fromFutureEitherA[A](f: Future[Either[Error, A]]): EitherT[Future, Error, A] = EitherT.apply(f)

    def isFriends2(user1: Long, user2: Long): EitherT[Future, Error, Boolean] = {

      for {
        followersOfUser1 <- fromFutureEitherA(followersF(user1))
        followersOfUser2 <- fromFutureEitherA(followersF(user2))
      } yield followersOfUser1.exists(_.id == user2) && followersOfUser2.exists(_.id == user1)

    }

    import scala.concurrent.duration._

    println(Await.result(isFriends1(0, 1), 1 second))
    println(Await.result(isFriends1(2, 1), 1 second))
    println(Await.result(isFriends1(0, 3), 1 second))
    println(Await.result(isFriends1(4, 3), 1 second))

    println(Await.result(isFriends2(0, 1).value, 1 second))
    println(Await.result(isFriends2(2, 1).value, 1 second))
    println(Await.result(isFriends2(0, 3).value, 1 second))
    println(Await.result(isFriends2(4, 3).value, 1 second))
  }
}