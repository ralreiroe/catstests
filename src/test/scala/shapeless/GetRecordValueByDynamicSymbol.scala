package shapeless

import cc.Spec


class GetRecordValueByDynamicSymbol extends Spec {

  "https://stackoverflow.com/questions/47105072/shapeless-record-get-value-by-symbol" in {

    import record._

    case class Dog(name: String, age: Int)
    val dog = Dog("rocky", 5)

    val repr = LabelledGeneric[Dog].to(dog)
    val nameWitn = Witness('name)

    repr.get(nameWitn)

//    def foo(fn: Symbol) = repr.get(Witness(fn))


  }
}
