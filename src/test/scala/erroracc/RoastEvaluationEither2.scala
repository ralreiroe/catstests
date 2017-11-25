package erroracc

import cc.Spec
import org.joda.time.LocalDate

import scala.util.{Either, Left, Right}

object RoastEvaluationEither2 {
  import EvaluationOptionFunctions._
  def evaluateRoast1(roast: Roast)( implicit now: () => LocalDate): Either[List[RoastProblem], SemiApprovedRoast] = {
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

  implicit def getDate() = LocalDate.parse("2017-11-4")

  """but what if I had several functions returning Either[List[RoastProblem], Roast].
    |Since Either is a monad
    |
    |https://stackoverflow.com/questions/21351391/how-to-accumulate-errors-in-either
    |
    |for-comprehensions (which desugar to a combination of calls to flatMap and map) are designed to allow you to sequence monadic computations in such a way that you have access to the result of earlier computations in subsequent steps.
    |
    |A for comprehension will "short-circuit" itself on the first error found, and this is almost always what you want.""".stripMargin in {

    val unevaluatedRoast = UnevaluatedRoast(level = RoastLevel.VeryLight, date = getDate().minusDays(14), isEven = false)

    RoastEvaluationEither2.evaluateRoast1(unevaluatedRoast) mustBe
      Left(List(
        RoastProblem("roast too light, at a 1"),
        RoastProblem("not fresh, roast date 2017-10-21 is more than 3 days old")))

    RoastEvaluationEither2.evaluateRoast2(unevaluatedRoast) mustBe
      Left(List(
        RoastProblem("roast is not evenly distributed")))


    def evaluate(roast: UnevaluatedRoast)(implicit now: () => LocalDate) = {
      val res: Either[List[RoastProblem], Roast] = for {
        t1 <- RoastEvaluationEither2.evaluateRoast1(roast)
        t2 <- RoastEvaluationEither2.evaluateRoast2(roast)
      } yield t2

      if(res.isRight) ApprovedRoast(roast.level, roast.date, roast.isEven) else res
    }

    evaluate(unevaluatedRoast) mustBe RoastEvaluationEither2.evaluateRoast1(unevaluatedRoast) //evaluateRoast2 is not evaluated - Short-circuiting is unwanted here; we don't get all errors

    val unevaluatedRoast2 = UnevaluatedRoast(level = RoastLevel.Dark, date = getDate().minusDays(2), isEven = false)


    evaluate(unevaluatedRoast2) mustBe RoastEvaluationEither2.evaluateRoast2(unevaluatedRoast)  //evaluateRoast2 is evaluated because evaluateRoast1 is a Right



  }

}
