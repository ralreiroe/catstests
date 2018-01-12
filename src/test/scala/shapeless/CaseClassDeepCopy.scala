package shapeless

import cc.Spec
import shapeless.labelled.FieldType

/**
  * https://stackoverflow.com/questions/33858745/automatically-convert-a-case-class-to-an-extensible-record-in-shapeless
  */
class CaseClassDeepCopy extends Spec {

  case class Address(street: String, zip: Int)
  case class Person(name: String, address: Address)

  val person = Person("Jane", Address("street address", 12345))
  val address = Address("street address", 12345)

  "shallow with explicit LabelledGeneric" in {

    LabelledGeneric[Address].to(address) mustBe "street address" :: 12345 :: HNil
    LabelledGeneric[Person].to(person) mustBe "Jane" :: Address("street address", 12345) :: HNil

  }

  "shallow with type alias" in {

    type ShallowPersonRec =
      FieldType[Witness.`'name`.T, String] ::
        FieldType[Witness.`'address`.T, Address] :: HNil

    val shallowrec: ShallowPersonRec = LabelledGeneric[Person].to(person)

    shallowrec mustBe "Jane" :: Address("street address", 12345) :: HNil

  }

  "shallow with implicit LabelledGeneric" in {

    def shallowRec[A](a: A)(implicit gen: LabelledGeneric[A]): gen.Repr = gen.to(a)

    shallowRec(address) mustBe "street address" :: 12345 :: HNil
    shallowRec(person) mustBe "Jane" :: Address("street address", 12345) :: HNil

  }



  "deep" in {

    //TODO

//  * https://stackoverflow.com/questions/33858745/automatically-convert-a-case-class-to-an-extensible-record-in-shapeless

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
