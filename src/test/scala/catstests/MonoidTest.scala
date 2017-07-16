package catstests

import cats.Monoid
import org.scalatest.{FlatSpec, Matchers}


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
    def combine(x: ValidationResult, y: ValidationResult): ValidationResult = (x, y) match {
      case (Valid, Valid) => Valid
      case (i @ Invalid(_), _) => i
      case (_, i @ Invalid(_)) => i
    }
  }
}

sealed trait Expr {
  def validate(fieldNames: List[String]): ValidationResult = {

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = field1.validate(fieldNames)
      val checkField2 = field2.validate(fieldNames)
      val monoid: Monoid[ValidationResult] = Monoid[ValidationResult]                         //<======= (1) is required here
      monoid.combineAll(List(checkField1, checkField2))
    }

    this match {
      case Add(field1, field2) => checkFields(field1, field2)
      case Multiply(field1, field2) => checkFields(field1, field2)
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

final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class FormCtx(value: String) extends Expr
final case class AuthCtx(value: AuthInfo) extends Expr
final case class EeittCtx(value: Eeitt) extends Expr
final case class Constant(value: String) extends Expr


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

  "" should "" in {
    println(Add(FormCtx("abc"), FormCtx("def")).validate(List("abc", "def")))
    println(Add(FormCtx("abd"), FormCtx("def")).validate(List("abc", "def")))
    println(Add(FormCtx("abd"), FormCtx("ded")).validate(List("abc", "def")))
  }


}
