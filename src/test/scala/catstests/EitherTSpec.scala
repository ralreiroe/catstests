package catstests

import org.scalatest.{ FlatSpec, Matchers }
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.concurrent.ScalaFutures
import cats.data.EitherT
import cats.instances.future._

class EitherTSpec extends FlatSpec with Matchers with ScalaFutures {

  // final case class EitherT[F[_], A, B](value: F[Either[A, B]]) { ... }

  "EitherT" should "x" in {

    val e: Either[String, Int] = Right(1)

    val f: Future[Int] = Future.successful(123)

    val et: EitherT[Future, String, Int] = EitherT(Future.successful(e))

    val ft: EitherT[Future, String, Int] = EitherT(f.map(i => Right(i): Either[String, Int]))

    val res = for {
      e <- et
      f <- ft
    } yield e + f

    println("res " + res.value.futureValue)
  }
}