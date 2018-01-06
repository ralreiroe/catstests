package typeclasses.vanilla


import org.scalatest._

class TypeClassHtmlWriterTest6 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {

    def toHtml4[A](value: A)(implicit w: A => String) = w(value)


    implicit val writeString: BoxedString => String = bs => bs.value.replaceAll("<", "&lt").replaceAll(">", "&gt")

    implicit val writeEmail: Email => String = value => value.email.replaceAll("@", " atat ")

    implicit val writePerson: Person => String = value => toHtml4(value.name) + " " + toHtml4(value.email)








    case class BoxedString(value: String)
    case class Email(email: String)
    case class Person(name: BoxedString, email: Email)

    println(toHtml4(Email("a@b")))
    println(toHtml4(BoxedString("<oops>")))
    println(toHtml4(Person(BoxedString("<Gene"), Email("a@z"))))




  }
}




