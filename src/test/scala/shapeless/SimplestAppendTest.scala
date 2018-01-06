package shapeless

import cc.Spec
import shapeless.labelled.FieldType
import shapeless.syntax.SingletonOps

case class One(fname: String, lname: String)

case class Two(name: String)

/**
  * https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#extensible-records
  */
class SimplestAppendTest extends Spec {

  "kejsjf" in {

    import shapeless._ ; import syntax.singleton._ ; import record._

    val book =
      ("author" ->> "Benjamin Pierce") ::
        ("title"  ->> "Types and Programming Languages") ::
        ("id"     ->>  262162091) ::
        ("price"  ->>  44.11) ::
        HNil



    val extended = book + ("inPrint" ->> true)


    println(extended.keys)
    println(extended)



  }


  "symbols" in {

    import shapeless._ ; import syntax.singleton._ ; import record._

    val book: ::[FieldType[SingletonOps#T, String], ::[FieldType[SingletonOps#T, String], ::[FieldType[SingletonOps#T, Int], ::[FieldType[SingletonOps#T, Double], HNil]]]] =
      ('author ->> "Benjamin Pierce") ::
        ('title  ->> "Types and Programming Languages") ::
        ('id     ->>  262162091) ::
        ('price  ->>  44.11) ::
        HNil



    val extended = book + ('inPrint ->> true)


    println(extended.keys)
    println(extended)





  }

}
