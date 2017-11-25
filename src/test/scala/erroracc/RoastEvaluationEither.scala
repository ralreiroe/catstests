package erroracc

import cc.Spec
import org.joda.time.LocalDate

import scala.util.{Either, Left, Right}

object RoastEvaluationEither {
  import EvaluationOptionFunctions._
  def evaluateRoast(roast: Roast): Either[List[RoastProblem], ApprovedRoast] = {
    val problems: List[RoastProblem] = List(
      evaluateRoastLevel(roast.level),
      evaluateFreshness(roast.date, LocalDate.now),
      evaluateEvenness(roast.isEven)).flatten //list of maybeRoastProblems flattened

    if (problems.isEmpty)
      Right(ApprovedRoast(roast.level, roast.date, roast.isEven))
    else
      Left(problems)
  }

}

class ErrorAccumulation extends Spec {

  "error acc" in {

    val roastProblemsOrApprovedRoast = RoastEvaluationEither.evaluateRoast(UnevaluatedRoast(level = RoastLevel.VeryLight, date = getDate.minusDays(14), isEven = false))

    val expected = Left(List(RoastProblem("roast too light, at a 1"), RoastProblem("not fresh, roast date 2017-10-21 is more than 3 days old"), RoastProblem("roast is not evenly distributed")))

    roastProblemsOrApprovedRoast mustBe expected


    roastProblemsOrApprovedRoast.left.get foreach println

    expected.left.get foreach println
  }


  private def getDate = {
    LocalDate.parse("2017-11-4")
  }
}
