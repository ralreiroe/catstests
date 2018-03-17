package catstests.monadtransformers

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

class EitherTSpec3 extends FlatSpec with Matchers with ScalaFutures {

//  http://eed3si9n.com/herding-cats/stacking-future-and-either.html
  "EitherT" should "x" in {

    case class User(id: Long, name: String)

    // In actual code, probably more than 2 errors
    sealed trait Error
    object Error {
      final case class UserNotFound(userId: Long) extends Error
      final case class ConnectionError(message: String) extends Error
    }
    object UserRepo {
      def followers(userId: Long): Either[Error, List[User]] = {

        userId match {
          case 0L => Right(List(User(1, "Michael")))
          case 1L => Right(List(User(0, "Vito")))
          case x => Left(Error.UserNotFound(x))
        }

      }
    }

    import Error._
    import UserRepo.followers

    def followEachOther(user1: Long, user2: Long): Either[Error, Boolean] =
      for {
        (followersOfUser1) <- followers(user1)        //pick out right from the Either
        followersOfUser2 <- followers(user2)
      } yield followersOfUser1.exists(_.id == user2) && followersOfUser2.exists(_.id == user1)

    followEachOther(0L, 1L) shouldBe Right(true)
    followEachOther(2L, 1L) shouldBe Left(UserNotFound(2))
    followEachOther(0L, 3L) shouldBe Left(UserNotFound(3))
    followEachOther(4L, 3L) shouldBe Left(UserNotFound(4))


    def followEachOtherFlatmap(user1: Long, user2: Long) = {
      followers(user1).flatMap {
        case (followersOfUser1) => {
          for (followersOfUser2 <- followers(user2)) yield followersOfUser1.exists(_.id == user2) && followersOfUser2.exists(_.id == user1)
          //see isFriends02:          followers(user2).map( followersOfUser2 => followersOfUser1.exists(_.id == user2) && followersOfUser2.exists(_.id == user1))
        }
      }
    }

    followEachOtherFlatmap(0L, 1L) shouldBe Right(true)
    followEachOtherFlatmap(2L, 1L) shouldBe Left(UserNotFound(2))
    followEachOtherFlatmap(0L, 3L) shouldBe Left(UserNotFound(3))
    followEachOtherFlatmap(4L, 3L) shouldBe Left(UserNotFound(4))


    def followEachOtherFlatmapMap(user1: Long, user2: Long) = {
      followers(user1).flatMap {
        case (followersOfUser1) => {
          followers(user2).map(followersOfUser2 => followersOfUser1.exists(_.id == user2) && followersOfUser2.exists(_.id == user1))
        }
      }
    }

    followEachOtherFlatmapMap(0L, 1L) shouldBe Right(true)
    followEachOtherFlatmapMap(2L, 1L) shouldBe Left(UserNotFound(2))
    followEachOtherFlatmapMap(0L, 3L) shouldBe Left(UserNotFound(3))
    followEachOtherFlatmapMap(4L, 3L) shouldBe Left(UserNotFound(4))

  }
}