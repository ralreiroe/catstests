package typeclasses.vanilla


import org.scalatest._

class TypeClassHtmlWriterTest31 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    trait HtmlWriter[A] {
      def write(value: A): String
    }


    case class Email(email: String)

    case class Person(name: String, email: Email)


    def toHtml4[A](value: A)(implicit w: HtmlWriter[A]) = w.write(value)

    object HtmlWriter {
      def apply[A](func: A => String) = new HtmlWriter[A] {
        override def write(value: A): String = func(value)
      }
    }

    implicit val stringWriter = HtmlWriter{
      value: String => value.replaceAll("<", "&lt").replaceAll(">", "&gt")
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




