package shapeless

import cc.Spec

class CaseClassCopy extends Spec {

  "https://stackoverflow.com/questions/23192760/safely-copying-fields-between-case-classes-of-different-types" in {

//    scala> import shapeless._
//    import shapeless._
//
//    scala> case class Test1(a: String, b: Int, c: Char)
//    defined class Test1
//
//    scala> case class Test2(a: String, b: Int)
//    defined class Test2
//
//    scala> val test1 = Test1("first", 2, '3')
//    test1: Test1 = Test1(first,2,3)
//
//    scala> val test2 = Test2("1st", 20)
//    test2: Test2 = Test2(1st,20)
//
//    scala> val test1Gen = Generic[Test1]
//    test1Gen: ... = $1$$1@6aac621f
//
//    scala> val test2Gen = Generic[Test2]
//    test2Gen: ... = $1$$1@5aa4954c
//
//    scala> val test3 = test1Gen.from(test2Gen.to(test2) :+ test1.c)
//    test3: Test1 = Test1(1st,20,3)


"""Note that this makes assumptions about the order of fields in each of the case classes rather than making use of field label information. This could be error prone where you had multiple fields of the same type: the types might line up, but the latent semantics might be altered.
  |
  |We can fix that by using shapeless's LabelledGeneric. LabelledGeneric maps case class values to shapeless extensible records which, as well as capturing the types of the field values, also encodes the field names in the type by way of the singleton type of the corresponding Scala Symbol. With a little bit of additional infrastructure (which I'll be adding to shapeless 2.1.0 shortly) this allows us to map between case classes safely with minimal boilerplate,...."""

  }

}
