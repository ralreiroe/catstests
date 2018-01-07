package shapeless

import cc.Spec

class CaseClassToExtensibleRecordAndBack extends Spec {

  "to ExtensibleRecord" in {

    case class UserWithAge(name: String, age: Int)
    val gen = LabelledGeneric[UserWithAge]
    val u = UserWithAge("Julien", 30)

    val h = gen.to(u) // returns Julien :: 30 :: HNil
    println(h)

    println(gen.from(h)) // return UserWithAge("Julien", 30)


    import record._
    import syntax.singleton._
    val extended = h + ('id ->> 123456789L)

    case class UserWithAgeAndId(name: String, age: Int, id: Long)
    val gen2 = LabelledGeneric[UserWithAgeAndId]

    println(gen2.from(extended))

  }

}
