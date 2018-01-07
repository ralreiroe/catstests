package shapeless

import cc.Spec
import shapeless.labelled.FieldType

class CaseClassToHList extends Spec {

  "to HList" in {

    import shapeless.Generic

    case class UserWithAge(name: String, age: Int)
    val gen = Generic[UserWithAge]
    val u = UserWithAge("Julien", 30)

    val h: ::[String, ::[Int, HNil]] = gen.to(u) // returns Julien :: 30 :: HNil
    h mustBe "Julien" :: 30 :: HNil
    gen.from(h) mustBe UserWithAge("Julien", 30)
  }

  "to ExtensibleRecord" in {

    import shapeless.Generic

    case class UserWithAge(name: String, age: Int)
    val gen = LabelledGeneric[UserWithAge]
    val u = UserWithAge("Julien", 30)

    val h = gen.to(u) // returns Julien :: 30 :: HNil
    println(h)
    println(gen.from(h)) // return UserWithAge("Julien", 30)

    h mustBe "Julien" :: 30 :: HNil
  }

}
