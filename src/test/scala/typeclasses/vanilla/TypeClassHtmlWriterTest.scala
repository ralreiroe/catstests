package typeclasses.vanilla


import org.scalatest._

class TypeClassHtmlWriterTest extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    //we have an abstract "write" function that can turn any type into an html string

    trait HtmlWriter[A] {
      def write(value: A): String
    }

    //implementations for String...

    object StringWriter extends HtmlWriter[String] {
      override def write(value: String): String = value.replaceAll("<", "&lt").replaceAll(">", "&gt")
    }

    println(StringWriter.write("<blah>"))

    //for Email...

    case class Email(email: String)

    object EmailWriter extends HtmlWriter[Email] {
      //...doing something and delegating the rest to the StringWriter implementation
      override def write(value: Email): String = StringWriter.write(value.email.replaceAll("@", " at "))
    }

    //for Person... again delegating to the other implementations

    case class Person(name: String, email: Email)

    object PersonHtmlWriter extends HtmlWriter[Person] {
      override def write(value: Person): String = StringWriter.write(value.name) + " " + EmailWriter.write(value.email)
    }

    //We don't want to explicitely specify the other implementations
    //instead, we would just like to specify take this A and convert it to an html String

    //like so:...

    def toHtml[A](value: A) = ""

    object PersonHtmlWriter2 extends HtmlWriter[Person] {
      override def write(value: Person): String = toHtml(value.name) + " " + toHtml(value.email)
    }

    //but how...?

    def toHtml2[A](value: A)(w: HtmlWriter[A]) = w.write(value)

    object PersonHtmlWriter3 extends HtmlWriter[Person] {
      override def write(value: Person): String = toHtml2(value.name)(StringWriter) + " " + toHtml2(value.email)(EmailWriter)
    }

    println(PersonHtmlWriter3.write(Person("Fred", Email("a@b"))))

    //Answer: make the HtmlWriter implicit

    implicit object StringWriter2 extends HtmlWriter[String] {
      override def write(value: String): String = value.replaceAll("<", "&lt").replaceAll(">", "&gt")
    }
    implicit object EmailWriter2 extends HtmlWriter[Email] {
      override def write(value: Email): String = StringWriter.write(value.email.replaceAll("@", " at "))
    }

    def toHtml3[A](value: A)(implicit w: HtmlWriter[A]): String = w.write(value)

    //Now this works because the compiler finds the functions it needs in scope:

    object PersonHtmlWriter4 extends HtmlWriter[Person] {
      override def write(value: Person): String = toHtml3(value.name) + " " + toHtml3(value.email)
    }

    println(PersonHtmlWriter4.write(Person("Oscar", Email("a@b"))))


    //by the way, we don't have to have named objects, we can also just create a instance and assign
    //it to an implicit val:

    implicit val emailWriter = new HtmlWriter[Email] {
      override def write(value: Email): String = StringWriter.write(value.email.replaceAll("@", " atat "))
    }

    def toHtml4[A](value: A)(implicit w: HtmlWriter[A]) = w.write(value)

    implicit object PersonHtmlWriter5 extends HtmlWriter[Person] {
      override def write(value: Person): String = toHtml4(value.name) + " " + toHtml4(value.email)
    }

    println(PersonHtmlWriter5.write(Person("Oscar", Email("a@b"))))

    println(toHtml4(Person("Mark", Email("a@z"))))

  }
}




