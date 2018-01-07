package shapeless

import cc.Spec

class CaseClassDeepCopy extends Spec {

  "https://stackoverflow.com/questions/33858745/automatically-convert-a-case-class-to-an-extensible-record-in-shapeless" in {


//
//    import shapeless._, shapeless.labelled.{ FieldType, field }
//
//    case class Address(street: String, zip: Int)
//    case class Person(name: String, address: Address)
//
//    val person = Person("Jane", Address("street address", 12345))
//
//    type ShallowPersonRec =
//      FieldType[Witness.`'name`.T, String] ::
//        FieldType[Witness.`'address`.T, Address] :: HNil
//
//    type DeepPersonRec =
//      FieldType[Witness.`'name`.T, String] ::
//        FieldType[
//          Witness.`'address`.T,
//          FieldType[Witness.`'street`.T, String] ::
//            FieldType[Witness.`'zip`.T, Int] :: HNil
//          ] :: HNil
//
//    val shallow: ShallowPersonRec = LabelledGeneric[Person].to(person)

  }




}
