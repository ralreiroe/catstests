package erroracc

import org.joda.time.LocalDate

object EvaluationOptionFunctions {
  def maybeLevelProblem(roastLevel: RoastLevel): Option[RoastProblem] = {
    if (roastLevel.value > 2)
      None
    else
      Some(RoastProblem(s"roast too light, at a ${roastLevel.value}"))
  }

  def maybeFreshnessProblem(roastDate: LocalDate)(implicit now: () => LocalDate): Option[RoastProblem] = {
    if (roastDate.isAfter(now().minusDays(3)))
      None
    else
      Some(RoastProblem(s"not fresh, roast date ${roastDate} is more than 3 days old"))
  }

  def maybeEvennessProblem(roastIsEven: Boolean): Option[RoastProblem] = {
    if (roastIsEven)
      None
    else
      Some(RoastProblem("roast is not evenly distributed"))
  }
}