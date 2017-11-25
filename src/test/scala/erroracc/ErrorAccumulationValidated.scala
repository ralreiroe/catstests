package erroracc

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import org.joda.time.LocalDate

object ComposingErrorsUsingValidated {

  /**
    *
    *  type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]
    *
    *  And Validated is an Applicative Functor: It has an ap method: see cats.data.Validated

  /**
    * From Apply:
    * if both the function and this value are Valid, apply the function
    */
  def ap[EE >: E, B](f: Validated[EE, A => B])(implicit EE: Semigroup[EE]): Validated[EE, B] =
    (this, f) match {
      case (Valid(a), Valid(f)) => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e2, e1))    //<==== combines errors, unlike a monadic flatmap which is designed to

      case (e@Invalid(_), _) => e
      case (_, e@Invalid(_)) => e
    }
    */
  def evaluateRoastLevel(roastLevel: RoastLevel): ValidatedNel[RoastProblem, RoastLevel] = {
    if (roastLevel.value > 2)
      Validated.valid(roastLevel)
    else
      Validated.invalidNel(RoastProblem(s"roast too light, at a ${roastLevel.value}"))
  }

  def evaluateFreshness(roastDate: LocalDate): ValidatedNel[RoastProblem, LocalDate] = {
    if (roastDate.isAfter(LocalDate.now.minusDays(3)))
      Validated.valid(roastDate)
    else
      Validated.invalidNel(RoastProblem(s"not fresh, roast date ${roastDate} is more than 3 days old"))
  }

  def evaluateEvenness(roastIsEven: Boolean): ValidatedNel[RoastProblem, Boolean] = {
    if (roastIsEven)
      Validated.valid(true)
    else
      Validated.invalidNel(RoastProblem("roast is not evenly distributed"))
  }

  def evaluateRoast1(roast: Roast): ValidatedNel[RoastProblem, SemiApprovedRoast] = {
    val ab = evaluateRoastLevel(roast.level) |@| evaluateFreshness(roast.date)
    (ab) map { (roastLevel: RoastLevel, date: LocalDate) =>
      SemiApprovedRoast(roast.level, roast.date, roast.isEven)
    }
  }
  def evaluateRoast2(roast: Roast): ValidatedNel[RoastProblem, SemiApprovedRoast] = {
    val ab = evaluateEvenness(roast.isEven)
    (ab) map { (evenNess: Boolean) =>
      SemiApprovedRoast(roast.level, roast.date, roast.isEven)
    }
  }
}

class ComposingErrorsUsingValidated extends cc.Spec {

  """error acc via Validated
    |I can take my approved roast and apply more validation functions to further scrutinize, grade, or otherwise classify roasts.
    |tack on more error accumulating functions
  """.stripMargin in {

    val unevaluatedRoast = UnevaluatedRoast(level = RoastLevel.VeryLight, date = LocalDate.now().minusDays(14), isEven = false)

    val validatedNel1 = ComposingErrorsUsingValidated.evaluateRoast1(unevaluatedRoast)
    println(validatedNel1)
    val validatedNel2 = ComposingErrorsUsingValidated.evaluateRoast2(unevaluatedRoast)
    println(validatedNel2)
    println(((validatedNel1 |@| validatedNel2).map((a, b) => (a,b))))

  }


}
