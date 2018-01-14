package shapeless

import cc.Spec
import shapeless.labelled.FieldType
import shapeless.syntax.SingletonOps

/**
  * https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#extensible-records
  */
class SimplestAppendTest extends Spec {

  "extensible record with strings" in {

    import shapeless._ ; import syntax.singleton._ ; import record._

    val book =
      ("author" ->> "Benjamin Pierce") ::
        ("title"  ->> "Types and Programming Languages") ::
        ("id"     ->>  262162091) ::
        ("price"  ->>  44.11) ::
        HNil



    val extended = book + ("inPrint" ->> true)

    val author = "author"


    import shapeless._, record._, syntax.singleton._

    val w = Witness("author")

    def fun[L <: HList](xs: L)(implicit sel: ops.record.Selector[L, w.T]) = xs("author")


println(fun(book))

    println(extended.keys)
    println(extended)

//    case class Book(author: String, title: String, id: Long, price: Double)
//
//
//
//    LabelledGeneric[Book].from(book)


  }


  "symbols" in {

    import shapeless._ ; import syntax.singleton._

    val book =
      ('author ->> "Benjamin Pierce") ::
        ('title  ->> "Types and Programming Languages") ::
        ('id     ->>  262162091) ::
        ('price  ->>  44.11) ::
        HNil

    import record._

    book('author) mustBe "Benjamin Pierce"
    val book2 = book.updated('author, "B")
    book2('author) mustBe "B"


    val extended = book + ('inPrint ->> true)


    println(extended.keys)
    println(extended)



    case class Book(author: String, title: String, id: Long, price: Double)




  }


  case class One(fname: String, lname: String)

  case class Two(name: String)



}
