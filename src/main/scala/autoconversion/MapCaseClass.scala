package autoconversion


import shapeless.LabelledGeneric.Aux
import shapeless.{::, HList, HNil, LabelledGeneric, Witness, labelled, tag}
import shapeless.labelled.FieldType
import shapeless.ops.record.Selector

trait ListBuilder[SourceHList, TargetHList] {
  def apply(source: SourceHList): TargetHList
}

object ListBuilder {
  implicit def empty[FromList] =

    new ListBuilder[FromList, HNil] {
      def apply(source: FromList): HNil = HNil
    }

  implicit def nonEmpty[TFn, TValueType, FromList <: HList, ToTailList <: HList](
                                                                              implicit
                                                                              select: Selector.Aux[FromList, TFn, TValueType],
                                                                              listBuilder: ListBuilder[FromList, ToTailList]
                                                                            )

  = new ListBuilder[FromList, FieldType[TFn, TValueType] :: ToTailList] {

    def apply(source: FromList) = {

      val srcFnAndType: FieldType[TFn, TValueType] = select(source).asInstanceOf[FieldType[TFn, TValueType]]
      srcFnAndType :: listBuilder(source)
    }
  }
}

trait CaseClassMap[Source, Target] {
  def apply(source: Source): Target
}

object CaseClassMap {
  implicit def caseClassMap[FromType, ToType, FromList, ToList](
                                                                 implicit
                                                                 fromlgen: LabelledGeneric.Aux[FromType, FromList],
                                                                 tolgen: LabelledGeneric.Aux[ToType, ToList],
                                                                 listBuilder: ListBuilder[FromList, ToList]
                                                               ) =

    new CaseClassMap[FromType, ToType] {
      def apply(inst: FromType) = {
        val listInst: ToList = listBuilder(fromlgen.to(inst))
        tolgen.from(listInst)
      }
    }
}

object MapCaseClass extends App {

  implicit class MappedToSyntax[From](val inst: From) {
    def mappedTo[To](implicit ccMap: CaseClassMap[From, To]): To = ccMap(inst)
  }

  case class SourceCC(fname: String, less18: Boolean)
  case class TargetCC(less18: Boolean)

  type FromList = FieldType[Witness.`'fname`.T, String] :: FieldType[Witness.`'less18`.T, Boolean] :: HNil
  type ToList = FieldType[Witness.`'less18`.T, Boolean] :: HNil

  private val fromlgen: Aux[SourceCC, FromList] = LabelledGeneric[SourceCC]
  private val tolgen: Aux[TargetCC, ToList] = LabelledGeneric[TargetCC]

  val targetInst: TargetCC = SourceCC("", false).mappedTo[TargetCC](
    CaseClassMap.caseClassMap(
      fromlgen,
      tolgen,
      ListBuilder.nonEmpty(
        Selector[FromList, Witness.`'less18`.T],
        ListBuilder.empty[FromList]))
  )
  println(TargetCC(false))



}