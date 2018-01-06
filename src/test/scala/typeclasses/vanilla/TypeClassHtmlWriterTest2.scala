package typeclasses.vanilla


import org.scalatest._

class TypeClassHtmlWriterTest2 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    //here are more of the implicit vals....

    trait HtmlWriter[A] {
      def write(value: A): String
    }


    case class Email(email: String)

    case class Person(name: String, email: Email)


    def toHtml4[A](value: A)(implicit w: HtmlWriter[A]) = w.write(value)

    implicit val stringWriter = new HtmlWriter[String] {
      override def write(value: String): String = value.replaceAll("<", "&lt").replaceAll(">", "&gt")
    }

    implicit val emailWriter = new HtmlWriter[Email] {
      override def write(value: Email): String = toHtml4(value.email.replaceAll("@", " atat "))
    }

    implicit val personWriter = new HtmlWriter[Person] {
      override def write(value: Person): String = toHtml4(value.name) + " " + toHtml4(value.email)
    }

    println(toHtml4(Person("Oscar", Email("a@b"))))

  }
}




