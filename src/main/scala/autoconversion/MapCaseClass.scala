package autoconversion


import shapeless.LabelledGeneric.Aux
import shapeless.{::, HList, HNil, LabelledGeneric, Witness}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.Selector

/**
  * Typeclass whose instances look for T2's head in T1
  */
trait HeadFinder[T1, T2] {
  def apply(source: T1): T2
}

object HeadFinder {
  implicit def empty[T1] =

    new HeadFinder[T1, HNil] {
      def apply(source: T1): HNil = HNil
    }

  implicit def nonEmpty[T1 <: HList, T2 <: HList, T3, T4](
                                                           implicit
                                                           select: Selector.Aux[T1, T3, T4],
                                                           headFinder: HeadFinder[T1, T2]
                                                         )

  = new HeadFinder[T1, FieldType[T3, T4] :: T2] { //<=================================================================

    def apply(from: T1) = field[T3].apply(select(from)) :: headFinder(from)
  }
}

trait CaseClasTransformer[T1, T2] {
  def apply(source: T1): T2
}

object CaseClasTransformer {
  implicit def transform[T1, T2, T3, T4](
                                          implicit
                                          fromlgen: LabelledGeneric.Aux[T1, T3],
                                          tolgen: LabelledGeneric.Aux[T2, T4],
                                          headFinder: HeadFinder[T3, T4]
                                        ) =

    new CaseClasTransformer[T1, T2] {
      def apply(inst: T1) = {
        val listInst: T4 = headFinder(fromlgen.to(inst))
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