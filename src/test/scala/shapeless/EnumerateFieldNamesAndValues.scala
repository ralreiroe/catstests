package shapeless

import cc.Spec
import shapeless.labelled.FieldType

class EnumerateFieldNamesAndValues extends Spec {


  "https://stackoverflow.com/questions/24647864/how-to-enumerate-shapeless-record-and-access-field-keys-in-runtime" in {
    case class One(fname: String, lname: String)

    object toNamedSingletonListOfValues extends Poly1 {
      implicit def caseField[K, T](implicit wk: Witness.Aux[K]) =
        at[FieldType[K, T]](field => {
          wk.value -> List[T](field)
        })
    }

    val generic = LabelledGeneric[One]
    val rec = generic.to(One("1", "2"))
    val values = rec.map(toNamedSingletonListOfValues)

    println(values)

    values mustBe ('fname,List("1")) :: ('lname,List("2")) :: HNil
  }

}
