package catstests

import cats.Monoid
import org.scalatest.{FlatSpec, Matchers}


sealed trait Expr

final case class Add(expr1: Expr, expr2: Expr) extends Expr
final case class Multiply(expr1: Expr, expr2: Expr) extends Expr
final case class FormCtx(value: String) extends Expr
final case class AuthCtx(value: AuthInfo) extends Expr
final case class EeittCtx(value: Eeitt) extends Expr
final case class Constant(value: String) extends Expr



sealed trait ValidationResult {
  type Opt[A] = Either[InvalidState, A]
  def toEither: Opt[Unit] = this match {
    case Valid => Right(())
    case Invalid(reason) => Left(InvalidState(reason))
  }
}

case object Valid extends ValidationResult
case class Invalid(reason: String) extends ValidationResult

object ValidationResult {

  implicit val validationResultMonoid = new Monoid[ValidationResult] {          //<======= implicit required (1)
    def empty: ValidationResult = Valid
    def combine(x: ValidationResult, y: ValidationResult): ValidationResult = (x, y) match {    //combining - accumulating reasons...
      case (Valid, Valid) => Valid
      case (Invalid(m1 @ _), Invalid(m2 @ _)) => Invalid(m1 + "|||" + m2)
      case (i @ Invalid(_), _) => i
      case (_, i @ Invalid(_)) => i
    }
  }
}

/**
  * But why monoids ? What do we stand to benefit from monoids ? Well, it turns out that we can write very interesting programs over any data type, knowing nothing about that type other than that it’s a monoid.
  */
object ExprLogic {

  val monoid: Monoid[ValidationResult] = Monoid[ValidationResult]                         //<======= (1) is required here

  def validate(expr: Expr, fieldNames: List[String]): ValidationResult = {

    expr match {
      case Add(expr1, expr2) => monoid.combineAll(List(validate(expr1, fieldNames), validate(expr2, fieldNames)))
      case Multiply(expr1, expr2) => monoid.combineAll(List(validate(expr1, fieldNames), validate(expr2, fieldNames)))
      case FormCtx(value) =>
        if (fieldNames.contains(value))
          Valid
        else
          Invalid(s"Form field '$value' is not defined in form template.")
      case AuthCtx(value) => Valid
      case EeittCtx(value) => Valid
      case Constant(_) => Valid
    }
  }
}


sealed trait Operation
final case object Addition extends Operation
final case object Multiplication extends Operation

sealed trait Eeitt
final case object BusinessUser extends Eeitt
final case object Agent extends Eeitt

sealed trait AuthInfo
final case object GG extends AuthInfo
final case object PayeNino extends AuthInfo
final case object SaUtr extends AuthInfo
final case object CtUtr extends AuthInfo



case class InvalidState(errorMsg: String)


class MonoidTest extends FlatSpec with Matchers {

  import ExprLogic.validate

  "combining validations" should "work" in {
    val listOfFieldNames = List("abc", "def")
    validate(Add(FormCtx("abc"), FormCtx("def")), listOfFieldNames) shouldBe(Valid)
    validate(Add(FormCtx("abd"), FormCtx("def")), listOfFieldNames) shouldBe(Invalid("Form field 'abd' is not defined in form template."))
    validate(Add(FormCtx("abd"), FormCtx("ded")), listOfFieldNames) shouldBe(Invalid("Form field 'abd' is not defined in form template.|||Form field 'ded' is not defined in form template."))


    validate(
      Add(
        Add(FormCtx("abd"), FormCtx("ded")),
        FormCtx("xyz")),
      listOfFieldNames) shouldBe(Invalid("Form field 'abd' is not defined in form template.|||Form field 'ded' is not defined in form template.|||Form field 'xyz' is not defined in form template.")
    )
  }


}
