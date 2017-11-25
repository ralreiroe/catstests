package erroracc

class Simple extends cc.Spec {

  case class Person(name: String, address: String, phone: String)

  def testPersonName(p: Person): Either[String, Person] = Left("name too short")
  def testAddress(p: Person) = Right(p)
  def testPhone(p: Person) = Left("invalid phone")

  "error accumulation by collecting the lefts from a seq of Eithers" in {

    //https://stackoverflow.com/questions/21351391/how-to-accumulate-errors-in-either

    def testPersonThroughSequentialExecutionRatherThanFlatmap(person: Person): Either[Seq[String], Person] = {
      val errstrOrPerson  = testPersonName(person)
      val errstr2OrPerson  = testAddress(person)
      val errstr3OrPerson = testPhone(person)

      val errors = Seq(errstrOrPerson, errstr2OrPerson, errstr3OrPerson) collect { case Left(err) => err }

      if(errors.isEmpty) Right(person) else Left(errors)
    }

    testPersonThroughSequentialExecutionRatherThanFlatmap(Person("sdfs", "sdfs", "sfdf")) mustBe Left(List("name too short", "invalid phone"))

  }

}
