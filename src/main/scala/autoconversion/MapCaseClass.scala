package autoconversion
//FOR IMPLICIT METHOD CALL INSERTS, LOOK AT THE *RETURN TYPES* NOTHING ELSE. LINES 29 AND 47

import shapeless.LabelledGeneric.Aux
import shapeless.{::, HList, HNil, LabelledGeneric, Witness}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.Selector

/**
  * Typeclass used to build an instance of H2 from elements of H1
  */
trait HeadFinder[H1, H2] {
  def apply(source: H1): H2
}

object HeadFinder {
  implicit def empty[H1] =

    new HeadFinder[H1, HNil] {
      def apply(source: H1): HNil = HNil
    }

  implicit def nonEmpty[H1 <: HList, H2 <: HList, K, V](
                                                           implicit
                                                           select: Selector.Aux[H1, K, V],
                                                           headFinder: HeadFinder[H1, H2]
                                                         )

  = new HeadFinder[H1, FieldType[K, V] :: H2] { //<=================================================================

    def apply(from: H1) = field[K].apply(select(from)) :: headFinder(from)
  }
}

trait CaseClasTransformer[T1, T2] {
  def apply(source: T1): T2
}

object CaseClasTransformer {
  implicit def transform[T1, T2, H1, H2](
                                          implicit
                                          fromlgen: LabelledGeneric.Aux[T1, H1],
                                          tolgen: LabelledGeneric.Aux[T2, H2],
                                          headFinder: HeadFinder[H1, H2]
                                        ) =

    new CaseClasTransformer[T1, T2] {
      def apply(inst: T1) = {
        val h1: H1 = fromlgen.to(inst)
        val listInst: H2 = headFinder(h1)
        tolgen.from(listInst)
      }
    }
}

object MapCaseClass extends App {

  implicit class TransformToSyntax[T1](val inst: T1) {
    def transformTo[T2](implicit cctransf: CaseClasTransformer[T1, T2]): T2 = cctransf(inst)
  }

  case class SourceCC(fname: String, less18: Boolean)

  case class TargetCC(less18: Boolean)

  type SourceL = FieldType[Witness.`'fname`.T, String] :: FieldType[Witness.`'less18`.T, Boolean] :: HNil
  type TargetL = FieldType[Witness.`'less18`.T, Boolean] :: HNil

  private val fromlgen: Aux[SourceCC, SourceL] = LabelledGeneric[SourceCC]
  private val tolgen: Aux[TargetCC, TargetL] = LabelledGeneric[TargetCC]

  val sourceInst = SourceCC("", false)
  val targetInst: TargetCC = sourceInst.transformTo[TargetCC](
    CaseClasTransformer.transform( //requires a HeadFinder[SourceL, TargetL] which can only be satisfied by insert a call to HeadFinder.nonEmpty (Line 29)
      fromlgen,
      tolgen,

      /**
        * HeadFinder.nonEmpty gives us a HeadFinder[T1, FieldType[T3, T4] :: T2]
        * The type parameters the compiler replaces by the real types SourceL and TargetL:
        * SourceL=T1, TargetL=FieldType[Witness.`'less18`.T, Boolean] :: HNil]
        */
      HeadFinder.nonEmpty(
        Selector[SourceL, Witness.`'less18`.T],
        HeadFinder.empty[SourceL]))
  )
  println(TargetCC(false))


  case class Class1(intValue: Int, stringValue: String)

  case class Class2(stringValue: String, intValue: Int)


  //  import MappedToSyntax._
  val res: Class2 = Class1(1, "quux").transformTo[Class2] // Class2("quux", 1)
  println(res)


  case class One2(name: String, less18: Boolean, more18: Boolean, fname: String)

  case class Two2(less18: Boolean, name: String, more18: Boolean)

  val res2: Two2 = One2("quux", false, true, "").transformTo[Two2]
  println(res2)


}