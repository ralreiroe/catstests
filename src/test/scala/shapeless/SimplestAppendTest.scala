package shapeless

import cc.Spec

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

    val book =
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
