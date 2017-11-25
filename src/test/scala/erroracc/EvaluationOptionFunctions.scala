package erroracc

import org.joda.time.LocalDate

object EvaluationOptionFunctions {
  def evaluateRoastLevel(roastLevel: RoastLevel): Option[RoastProblem] = {
    if (roastLevel.value > 2)
      None
    else
      Some(RoastProblem(s"roast too light, at a ${roastLevel.value}"))
  }

  def evaluateFreshness(roastDate: LocalDate, now: LocalDate): Option[RoastProblem] = {
    if (roastDate.isAfter(now.minusDays(3)))
      None
    else
      Some(RoastProblem(s"not fresh, roast date ${roastDate} is more than 3 days old"))
  }

  def evaluateEvenness(roastIsEven: Boolean): Option[RoastProblem] = {
    if (roastIsEven)
      None
    else
      Some(RoastProblem("roast is not evenly distributed"))
  }
}