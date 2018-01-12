package autoconversion


import shapeless.{HList, ::, HNil, LabelledGeneric}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.Selector


trait FieldsSelector[SourceHList, TargetHList] {
  def apply(source: SourceHList): TargetHList
}

object FieldsSelector {
  implicit def hNilFieldsSelector[SourceHList]: FieldsSelector[SourceHList, HNil] =
    new FieldsSelector[SourceHList, HNil] {
      def apply(dontCare: SourceHList): HNil = HNil
    }

  implicit def hListFieldsSelector[Value, Key, TargetHListTail <: HList, SourceHList <: HList](
    implicit
    restFieldsSelect: FieldsSelector[SourceHList, TargetHListTail],
    select: Selector.Aux[SourceHList, Key, Value] // select the value of type Value labelled with Key from a HList of type Source
  ): FieldsSelector[SourceHList, FieldType[Key, Value] :: TargetHListTail] =

    new FieldsSelector[SourceHList, FieldType[Key, Value] :: TargetHListTail] {
      def apply(source: SourceHList): FieldType[Key, Value] :: TargetHListTail =
        field[Key](select(source)) :: restFieldsSelect(source)
    }
}

trait CaseClassMap[Source, Target] {
  def apply(source: Source): Target
}

object CaseClassMap {
  implicit def caseClassMap[Source, Target, SourceRepr <: HList, TargetRepr <: HList](
     implicit
     sourceGen: LabelledGeneric.Aux[Source, SourceRepr],
     targetGen: LabelledGeneric.Aux[Target, TargetRepr],
     labelledHListMapper: FieldsSelector[SourceRepr, TargetRepr]
   ): CaseClassMap[Source, Target] =

    new CaseClassMap[Source, Target] {
      def apply(source: Source): Target = targetGen.from(labelledHListMapper(sourceGen.to(source)))
  }
}




object MapCaseClass extends App {

  implicit class MappedToSyntax[Source](val source: Source) {
    def mappedTo[Target](implicit ccMap: CaseClassMap[Source, Target]): Target = ccMap(source)
  }


  case class Class1(intValue: Int, stringValue: String)
  case class Class2(stringValue: String, intValue: Int)


  //  import MappedToSyntax._
  val res: Class2 = Class1(1, "quux").mappedTo[Class2] // Class2("quux", 1)
  println(res)

}