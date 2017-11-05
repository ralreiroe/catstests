package erroracc

import org.joda.time.LocalDate
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Either, Left, Right}

sealed abstract class RoastLevel(val value: Int)
object RoastLevel {
  case object VeryLight extends RoastLevel(1)
  case object Light     extends RoastLevel(2)
  case object Medium    extends RoastLevel(3)
  case object Dark      extends RoastLevel(4)
  case object Burnt     extends RoastLevel(5)
}

trait Roast {
  def level: RoastLevel
  def date: LocalDate
  def isEven: Boolean
}
case class UnevaluatedRoast(level: RoastLevel, date: LocalDate, isEven: Boolean) extends Roast
case class ApprovedRoast(level: RoastLevel, date: LocalDate, isEven: Boolean) extends Roast
case class SemiApprovedRoast(level: RoastLevel, date: LocalDate, isEven: Boolean) extends Roast

case class RoastProblem(reason: String)

