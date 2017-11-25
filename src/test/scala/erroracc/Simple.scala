package erroracc

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

class Simple extends cc.Spec {

  "error accumulation by collecting the lefts from a seq of Eithers - accumulation but not composable" in {

    case class Person(name: String, address: String, phone: String)

    def testPersonName(p: Person): Either[String, Person] = Left("name too short")

    def testAddress(p: Person) = Right(p)

    def testPhone(p: Person) = Left("invalid phone")

    //https://stackoverflow.com/questions/21351391/how-to-accumulate-errors-in-either

    def testPersonThroughSequentialExecutionRatherThanFlatmap(person: Person): Either[Seq[String], Person] = {
      val errstrOrPerson = testPersonName(person)
      val errstr2OrPerson = testAddress(person)
      val errstr3OrPerson = testPhone(person)

      val errors = Seq(errstrOrPerson, errstr2OrPerson, errstr3OrPerson) collect { case Left(err) => err }

      if (errors.isEmpty) Right(person) else Left(errors)
    }

    testPersonThroughSequentialExecutionRatherThanFlatmap(Person("sdfs", "sdfs", "sfdf")) mustBe Left(List("name too short", "invalid phone"))

  }

  case class Person(name: String, address: String, phone: String)

  def testPersonName(p: Person): Validated[String, Person] = Invalid("name too short")
  def testAddress(p: Person): Validated[String, Person] = Valid(p)
  def testPhone(p: Person): Validated[String, Person] = Invalid("invalid phone")

  val p = Person("", "", "")

  "error accumulation by composing a Set of Validateds" in {

    import cats.implicits._
    (testPersonName(p) |@| testAddress(p) |@| testPhone(p)).map((a, _, _) => (a)) mustBe Invalid("name too shortinvalid phone")

  }

  import cats.implicits._
  def testingFunction1(p: Person): Validated[String, Person] = (testPersonName(p) |@| testAddress(p) |@| testPhone(p)).map((a, _, _) => a)

  "now compose with further test functions easily" in {

    def testSSN(p: Person): Validated[String, Person] = Invalid("missing SSN") //a new testing function

    import cats.implicits._
    (testingFunction1(p) |@| testSSN(p)).map((a, _) => a) mustBe Invalid("name too shortinvalid phonemissing SSN")

  }

}
