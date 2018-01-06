package typeclasses.vanilla


import org.scalatest._
import typeclasses.TypeClassHtmlWriterTest7Model._
import typeclasses.TypeClassHtmlWriterTest7HtmlOperations._

class TypeClassHtmlWriterTest7 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {

    implicit val writeString: BoxedString => String = bs => bs.value.replaceAll("<", "&lt").replaceAll(">", "&gt")

    implicit val writeEmail: Email => String = value => value.email.replaceAll("@", " atat ")

    implicit val writePerson: SimplePerson => String = value => toHtml4(value.name) + " " + toHtml4(value.email)


    println(toHtml4(Email("a@b")))
    println(toHtml4(BoxedString("<oops>")))
    println(toHtml4(SimplePerson(BoxedString("<Gene"), Email("a@z"))))




  }
}




