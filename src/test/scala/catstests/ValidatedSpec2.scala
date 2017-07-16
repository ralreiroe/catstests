package catstests

import java.time.LocalDate

import cats.data.Validated
import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.cartesian._
import org.scalatest._

import scala.util.Try

class ValidatedSpec2 extends FlatSpec with Matchers with EitherValues {

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

  val missingYear: Option[String] => Validated[String, String] = yearStr => Validated.fromOption(yearStr, "Missing Year")
  val missingMonth: Option[String] => Validated[String, String] = yearStr => Validated.fromOption(yearStr, "Missing Month")

  val toSetOfString: String => Set[String] = error => Set(error)

  "Validated" should "validate date" in {

    val dataNoYear = Map(
      "day" -> "1",
      "month" -> "s2333"
//      ,
//      "year" -> "200000"
    )

    val res = missingYear(dataNoYear.get("year")).andThen(validNumber).andThen(validYear)
    println("res " + res)

    val dataYearNoNum = Map(
      "day" -> "1",
      "month" -> "s2333"
            ,
            "year" -> "s200000"
    )

    println("res " + missingYear(dataYearNoNum.get("year")).andThen(validNumber).andThen(validYear))

    val dataYearCorrect = Map(
      "day" -> "1",
      "month" -> "s2333"
            ,
            "year" -> "2005"
    )

    println("res " + missingYear(dataYearCorrect.get("year")).andThen(validNumber).andThen(validYear))

  }

  it should "validate year and month" in {

    val dataYearMonth= Map(
      "day" -> "1",
      "month" -> "s2333"
//      ,
//      "year" -> "2005"
    )

    val validatedYear: Validated[String, Int] = missingYear(dataYearMonth.get("year")).andThen(validNumber).andThen(validYear)
    val validatedMonth: Validated[String, Int] = missingMonth(dataYearMonth.get("month")).andThen(validNumber).andThen(validMonth)

    val res1: Validated[Set[String], Int] = validatedYear.leftMap((s: String) => Set(s))
    val res2: Validated[Set[String], Int] = validatedMonth.leftMap((s: String) => Set(s))

    val res = res1 |@| res2
    val res3: Validated[Set[String], (Int, Int)] = res.map((a, b) => (a,b))

//    val res = validatedYear.leftMap(_ => Set(_)) |@| validatedMonth


    println("cart " + res3)

  }
}