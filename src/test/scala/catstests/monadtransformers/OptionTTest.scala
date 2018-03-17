package catstests.monadtransformers

import cc.Spec

import scala.concurrent.Future


/**
  *
  * OptionT is a wrapper for Future[Option]
  *
  * https://blog.buildo.io/monad-transformers-for-the-working-programmer-aa7e981190e7
  *
  */
class OptionTTest extends Spec {


  case class User()
  case class Address()

  def findUserById(id: Long): Future[Option[User]] = ???
  def findAddressByUser(user: User): Future[Option[Address]] = ???

  "" in {

    def findAddressByUserId(id: Long): Future[Option[Address]] =
      findUserById(id).flatMap {
        case Some(user) => findAddressByUser(user)
        case None       => Future.successful(None)
      }

    case class OptionT[A](value: Future[Option[A]]) {

      def map[B](f: A => B): OptionT[B] =
        OptionT(value.map(optA => optA.map(f)))
      def flatMap[B](f: A => OptionT[B]): OptionT[B] =
        OptionT(value.flatMap(opt => opt match {
          case Some(a) => f(a).value
          case None => Future.successful(None)
        }))
    }

    def findAddressByUserIdUsingOptionT(id: Long): Future[Option[Address]] =
      (for {
        user    <- OptionT(findUserById(id))
        address <- OptionT(findAddressByUser(user))
      } yield address).value



  }

}
