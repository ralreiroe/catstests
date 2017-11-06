package erroracc

class Simple extends cc.Spec {

  case class Person(name: String, address: String, phone: String)

  def testPersonName(p: Person): Either[String, Person] = Left("name too short")
  def testAddress(p: Person) = Right(p)
  def testPhone(p: Person) = Left("invalid phone")

  "simple error accumulation" in {

    //https://stackoverflow.com/questions/21351391/how-to-accumulate-errors-in-either

    def testPersonThroughSequentialExecutionRatherThanFlatmap(person: Person): Either[List[String], Person] = {
      val name  = testPersonName(person)
      val addr  = testAddress(person)
      val phone = testPhone(person)

      val errors = List(name, addr, phone) collect { case Left(err) => err }

      if(errors.isEmpty) Right(person) else Left(errors)
    }

    testPersonThroughSequentialExecutionRatherThanFlatmap(Person("sdfs", "sdfs", "sfdf")) mustBe Left(List("name too short", "invalid phone"))

  }

}
