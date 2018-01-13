package autoconversion


import shapeless.{::, HList, HNil, LabelledGeneric}
import shapeless.labelled.{FieldType}
import shapeless.ops.record.Selector


trait ListBuilder[SourceHList, TargetHList] {
  def apply(source: SourceHList): TargetHList
}

object ListBuilder {
  implicit def emptyTgtListHeadExtractor[SourceHList] =

    new ListBuilder[SourceHList, HNil] {
      def apply(notNeeded: SourceHList): HNil = HNil
    }

  /**
    */

  implicit def nonEmptyTgtListHeadExtractor[TFn, TValueType, FromList <: HList, ToList <: HList](
                                                                                                  implicit
                                                                                                  listBuilder: ListBuilder[FromList, ToList]
                                                                                                  ,
                                                                                                  select: Selector.Aux[FromList, TFn, TValueType]
                                                                                                )

  = new ListBuilder[FromList, FieldType[TFn, TValueType] :: ToList] {

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
      def apply(inst: FromType) = tolgen.from(listBuilder(fromlgen.to(inst)))
    }
}

object MapCaseClass extends App {

  implicit class MappedToSyntax[From](val inst: From) {
    def mappedTo[To](implicit ccMap: CaseClassMap[From, To]): To = ccMap(inst)
  }

  case class SourceCC(fname: String, less18: Boolean, more18: Boolean, name: String)

  case class TargetCC(less18: Boolean, name: String, more18: Boolean)

  val res2: TargetCC = SourceCC("", false, true, "quux").mappedTo[TargetCC]
  println(res2)


}