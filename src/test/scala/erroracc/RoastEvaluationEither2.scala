package erroracc

import cc.Spec
import org.joda.time.LocalDate

import scala.util.{Either, Left, Right}

object RoastEvaluationEither2 {
  import EvaluationOptionFunctions._
  def evaluateRoast1(roast: Roast): Either[List[RoastProblem], SemiApprovedRoast] = {
    val problems: List[RoastProblem] = List(
      evaluateRoastLevel(roast.level),
      evaluateFreshness(roast.date)).flatten

    if (problems.isEmpty)
      Right(SemiApprovedRoast(roast.level, roast.date, roast.isEven))
    else
      Left(problems)
  }

  def evaluateRoast2(roast: Roast): Either[List[RoastProblem], Roast] = {
    val problems: List[RoastProblem] = List(
      evaluateEvenness(roast.isEven)).flatten

    if (problems.isEmpty)
      Right(SemiApprovedRoast(roast.level, roast.date, roast.isEven))
    else
      Left(problems)
  }

}

class ErrorAccumulation2 extends Spec {

  """but what if I had several Either[List[RoastProblem], Roast] returning functions.
    |Since Either is a monad
    |
    |for comprehensions don't really mix with error accumulations. In fact, you generally use them when you don't want fine grained error control.
    |
    |A for comprehension will "short-circuit" itself on the first error found, and this is almost always what you want.""".stripMargin in {

    val unevaluatedRoast = UnevaluatedRoast(level = RoastLevel.VeryLight, date = LocalDate.now().minusDays(14), isEven = false)
    RoastEvaluationEither2.evaluateRoast1(unevaluatedRoast) mustBe Left(List(RoastProblem("roast too light, at a 1"), RoastProblem("not fresh, roast date 2017-10-21 is more than 3 days old")))

    def evaluate(roast: UnevaluatedRoast) = {
      val res: Either[List[RoastProblem], Roast] = for {
        t1 <- RoastEvaluationEither2.evaluateRoast1(roast)
        t2 <- RoastEvaluationEither2.evaluateRoast2(roast)
      } yield t2

      if(res.isRight) ApprovedRoast(roast.level, roast.date, roast.isEven) else res
    }

    println(evaluate(unevaluatedRoast))

    val unevaluatedRoast2 = UnevaluatedRoast(level = RoastLevel.Dark, date = LocalDate.now().minusDays(2), isEven = false)
    println(evaluate(unevaluatedRoast2))



  }


}
