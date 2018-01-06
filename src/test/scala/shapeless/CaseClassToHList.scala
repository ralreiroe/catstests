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
    println(h)
    println(gen.from(h)) // return UserWithAge("Julien", 30)
  }

  "to ExtensibleRecord" in {

    import shapeless.Generic

    case class UserWithAge(name: String, age: Int)
    val gen = LabelledGeneric[UserWithAge]
    val u = UserWithAge("Julien", 30)

    val h: ::[FieldType[String with labelled.KeyTag[Symbol with tag.Tagged[{
  type name
}], String], String], ::[FieldType[Int with labelled.KeyTag[Symbol with tag.Tagged[{
  type age
}], Int], Int], HNil]] = gen.to(u) // returns Julien :: 30 :: HNil
    println(h)
    println(gen.from(h)) // return UserWithAge("Julien", 30)
  }

}
