package stringRows

import scala.util.Try

/**
  * Typeclass that converts a String to some other type T
  *
  * @tparam T
  */
trait StringConversion[T] {
  def apply(value: String): Option[T]
}
object StringConversion {
  implicit object convertToInt extends StringConversion[Int] {
    def apply(value: String) = Try(value.toInt).toOption
  }

  implicit object convertToString extends StringConversion[String] {
    def apply(value: String) = Some(value)
  }

  implicit object convertToDouble extends StringConversion[Double] {
    def apply(value: String) = Try(value.toDouble).toOption
  }

  implicit object convertToCountryCode extends StringConversion[CountryCode] {
    def apply(value: String) = CountryCode.createSafe(value)
  }

  implicit object convertToDateOfBirth extends StringConversion[DateOfBirth] {
    def apply(value: String) = DateOfBirth.createSafe(value)
  }
}

case class CountryCode(`val`: String)
//https://stackoverflow.com/questions/24002422/whats-the-correct-way-to-enforce-constraints-on-case-class-values
object CountryCode {
  def createSafe(value: String): Option[CountryCode] = {
    if (value.size==2) Some(CountryCode(value)) else None
  }
}
case class DateOfBirth(`val`: String)
object DateOfBirth {
  def createSafe(value: String): Option[DateOfBirth] = {
    if ("""\d{4}-\d{2}-\d{2}""".r.findAllIn(value).isEmpty) None else Some(DateOfBirth(value))
  }
}
