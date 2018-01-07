package shapeless

import cc.Spec

class CaseClassConversion2 extends Spec {

  "https://stackoverflow.com/questions/31949455/transform-one-case-class-into-another-when-the-argument-list-is-the-same" in {


    import shapeless._

    case class Create(userName: String, firstName: String, lastName: String)
    case class Created(userName: String, firstName: String, lastName: String)
    case class SortOfCreated(screenName: String, firstName: String, lastName: String)

    val c = Create("username", "firstname", "lastname")

    val createGen = LabelledGeneric[Create]
    val createdGen = LabelledGeneric[Created]
    val sortOfCreatedGen = LabelledGeneric[SortOfCreated]

    val created: Created = createdGen.from(createGen.to(c))

//    sortOfCreatedGen.from(createGen.to(c)) // fails to compile
  }

}
