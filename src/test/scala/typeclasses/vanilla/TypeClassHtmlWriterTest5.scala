package typeclasses.vanilla


import org.scalatest._

class TypeClassHtmlWriterTest5 extends FlatSpec with Matchers {
  "Hello" should "have tests" in {


    //do we actualler *need* an trait to use the write function?

//    trait HtmlWriter[A] {
//      def write(value: A): String
//    }

//    def toHtml4[A](value: A)(implicit writerTrait: HtmlWriter[A]) = writerTrait.write(value)

    //Or can we just use a 'freestanding' function w?

    
    def toHtml4[A](value: A)(w: A => String) = w(value)


    val writeString: String => String = value => value.replaceAll("<", "&lt").replaceAll(">", "&gt")

    val writeEmail: Email => String = value => toHtml4(value.email.replaceAll("@", " atat "))(writeString)

    val writePerson: Person => String = value => toHtml4(value.name)(writeString) + " " + toHtml4(value.email)(writeEmail)

    case class Email(email: String)

    case class Person(name: String, email: Email)



    println(toHtml4("<blah>")(writeString))

    println(toHtml4(Person("Oscar", Email("a@b")))(writePerson))

  }
}




