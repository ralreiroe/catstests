package catstests

import java.time.LocalDate

import cats.data.Validated
import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.cartesian._
import org.scalatest._

import scala.util.Try

case class FieldId(value: String) extends AnyVal
case class FieldValue(fieldId: FieldId)

class ValidatedSpec extends FlatSpec with Matchers with EitherValues {

  val validNumber: String => Validated[String, Int] = n => Validated.fromTry(Try(n.toInt)).leftMap(_ => s"$n is not a number")

  val validDay: Int => Validated[String, Int] = day => day match {
    case day if day < 1 => Validated.invalid("Day must be greater than 1")
    case day if day >= 31 => Validated.invalid("Day must be less than 31")
    case _ => Validated.valid(day)
  }

  val validMonth: Int => Validated[String, Int] = month => month match {
    case month if month < 1 => Validated.invalid("Month must be greater than 1")
    case month if month >= 12 => Validated.invalid("Month must be less than 12")
    case _ => Validated.valid(month)
  }

  val validYear: Int => Validated[String, Int] = year => year match {
    case year if year < 2000 => Validated.invalid("Year must be greater than 2000")
    case year if year >= 2017 => Validated.invalid("Year must be less than 2017")
    case _ => Validated.valid(year)
  }
//
//  def a1(data: Map[String, String], fieldValue: FieldValue): Validated[Map[FieldValue, Set[String]], Int] = {
//    val step1: Validated[Map[FieldValue, Set[String]], String] = Validated.fromOption(data.get("dob.year"), Map(fieldValue -> Set("Missing year")))
//
//    val step2: Validated[Map[FieldValue, Set[String]], Int] = step1.andThen{yearAsString =>
//      Validated.fromTry(Try(yearAsString.toInt)).leftMap(throwable => Map(fieldValue -> Set("Year must a number")))
//    }
//
//    val step3 = step2.andThen{number =>
//      if(number > 2017)
//        Validated.Invalid(Map(fieldValue -> Set("Year must a less than 2017")))
//      else
//        Validated.Valid(number)
//    }
//    step3
//  }

  //(Validated[Error, Int] |@| Validated[Error, String] |@| Validated[Error, Boolean]) map (Int, String, Boolean) => X  => Validated[Error, X]

  val validDate: FieldValue => Map[String, String] => Validated[Map[FieldId, Set[String]], LocalDate] = fieldValue => data => {

    val fieldId = fieldValue.fieldId
    (
      Validated.fromOption(data.get(fieldId.value + ".year"), "Missing year").andThen(validNumber).andThen(validYear).leftMap(toSetOfString) |@|
        Validated.fromOption(data.get(fieldId.value + ".month"), "Missing month").andThen(validNumber).andThen(validMonth).leftMap(toSetOfString) |@|
        Validated.fromOption(data.get(fieldId.value + ".day"), "Missing day").andThen(validNumber).andThen(validDay).leftMap(toSetOfString)
      ).map((a,b,c) => LocalDate.of(a, b, c)).leftMap(setString => bindFieldId(fieldValue)(setString))
  }

  val toSetOfString: String => Set[String] = error => Set(error)

  val bindFieldId: FieldValue => Set[String] => Map[FieldId, Set[String]] = fv => errors => Map(fv.fieldId -> errors)

  val fieldValue = FieldValue(FieldId("startDate"))
  val fieldValue2 = FieldValue(FieldId("endDate"))

  "Validated" should "validate date" in {

    val i1 = Map(
      //      "startDate.day" -> "1",
      "startDate.month" -> "s2333",
      "startDate.year" -> "200000"
    )

    val i2 = Map(
      "startDate.day" -> "a",
      "startDate.month" -> "200",
      "startDate.year" -> "11111"
    )

    val v1 = Map(
      "startDate.day" -> "1",
      "startDate.month" -> "2",
      "startDate.year" -> "2000"
    )

    val v2 = Map(
      "startDate.day" -> "3",
      "startDate.month" -> "4",
      "startDate.year" -> "2015"
    )


    val res1: Validated[Map[FieldId, Set[String]], Unit] = validDate(fieldValue)(i1).map(_ => ())
    val res2: Validated[Map[FieldId, Set[String]], Unit] = validDate(fieldValue)(i2).map(_ => ())

    val res3: Validated[Map[FieldId, Set[String]], Unit] =
      Monoid[Validated[Map[FieldId, Set[String]], Unit]].combineAll(List(res1, res2))

    println("res1 " + res1)
    println("res2 " + res2)
    println("res3 " + res3)

    val res4: Validated[Map[FieldId, Set[String]], Unit] = validDate(fieldValue)(v1).map(_ => ())
    val res5: Validated[Map[FieldId, Set[String]], Unit] = validDate(fieldValue)(v2).map(_ => ())
    val res6: Validated[Map[FieldId, Set[String]], Unit] =
      Monoid[Validated[Map[FieldId, Set[String]], Unit]].combineAll(List(res4, res5))

    println("res6 " + res6)
  }
}