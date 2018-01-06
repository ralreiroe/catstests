package typeclasses

package object TypeClassHtmlWriterTest7Model {

  //our types - look: they implement not a thing

  case class BoxedString(value: String)

  case class Email(email: String)

  case class SimplePerson(name: BoxedString, email: Email)




}