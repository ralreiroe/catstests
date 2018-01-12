package shapeless

import cc.Spec

class CaseClassDeepCopy extends Spec {

  "https://stackoverflow.com/questions/33858745/automatically-convert-a-case-class-to-an-extensible-record-in-shapeless" in {

    import shapeless._, shapeless.labelled.{ FieldType, field }

    case class Address(street: String, zip: Int)

    type ShallowPersonRec =
      FieldType[Witness.`'name`.T, String] ::
        FieldType[Witness.`'address`.T, Address] :: HNil

    case class Person(name: String, address: Address)
    val person = Person("Jane", Address("street address", 12345))

    val shallowrec: ShallowPersonRec = LabelledGeneric[Person].to(person)

    shallowrec mustBe "Jane" :: Address("street address",12345) :: HNil

    def shallowRec[A](a: A)(implicit gen: LabelledGeneric[A]): gen.Repr = gen.to(a)

    val shallowrec2 = shallowRec(person)

    shallowrec2 mustBe "Jane" :: Address("street address",12345) :: HNil


    //
//    type DeepPersonRec =
//      FieldType[Witness.`'name`.T, String] ::
//        FieldType[
//          Witness.`'address`.T,
//          FieldType[Witness.`'street`.T, String] ::
//            FieldType[Witness.`'zip`.T, Int] :: HNil
//          ] :: HNil
//

  }




}
