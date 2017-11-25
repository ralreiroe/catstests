package erroracc

import cc.Spec
import org.joda.time.LocalDate

import scala.util.{Either, Left, Right}

object RoastEvaluationEither2 {
  import EvaluationOptionFunctions._
  def problemsOrRoast(roast: Roast)(implicit now: () => LocalDate): Either[List[RoastProblem], SemiApprovedRoast] = {
    val problems: List[RoastProblem] = List(
      maybeLevelProblem(roast.level),
      maybeFreshnessProblem(roast.date)).flatten

    if (problems.isEmpty)
      Right(SemiApprovedRoast(roast.level, roast.date, roast.isEven))
    else
      Left(problems)
  }

  def moreProblemsOrRoast(roast: Roast): Either[List[RoastProblem], Roast] = {
    val problems: List[RoastProblem] = List(
      maybeEvennessProblem(roast.isEven)).flatten

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

    RoastEvaluationEither2.problemsOrRoast(unevaluatedRoast) mustBe
      Left(List(
        RoastProblem("roast too light, at a 1"),
        RoastProblem("not fresh, roast date 2017-10-21 is more than 3 days old")))

    RoastEvaluationEither2.moreProblemsOrRoast(unevaluatedRoast) mustBe
      Left(List(
        RoastProblem("roast is not evenly distributed")))


    def combinedProblemsOrRoast(roast: UnevaluatedRoast)(implicit now: () => LocalDate) = {
      val res: Either[List[RoastProblem], Roast] = for {
        t1 <- RoastEvaluationEither2.problemsOrRoast(roast)
        t2 <- RoastEvaluationEither2.moreProblemsOrRoast(roast)
      } yield t2

      if(res.isRight) ApprovedRoast(roast.level, roast.date, roast.isEven) else res
    }

    combinedProblemsOrRoast(unevaluatedRoast) mustBe RoastEvaluationEither2.problemsOrRoast(unevaluatedRoast) //moreProblemsOrRoast is not evaluated - Short-circuiting is unwanted here; we don't get all errors

    val unevaluatedRoast2 = UnevaluatedRoast(level = RoastLevel.Dark, date = getDate().minusDays(2), isEven = false)


    combinedProblemsOrRoast(unevaluatedRoast2) mustBe RoastEvaluationEither2.moreProblemsOrRoast(unevaluatedRoast)  //moreProblemsOrRoast is evaluated because problemsOrRoast is a Right



  }

}
