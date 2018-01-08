package shapeless

import cc.Spec

class ForgetHolder extends Spec {

  case class One(fname: String, lname: String, age: Int)

  import record._

  val one = One("1", "2", 44)
  val gen = LabelledGeneric[One]
  var h = gen.to(one)

  val age: Int = h('age)
  val fn: String = h('fname)
  val ln: String = h('lname)

  val h3 = h.updated('fname, ln).updated('lname, fn)

  import syntax.singleton._

  val j =
    ('fname ->> "3") ::
      ('lname  ->> "4") ::
      ('age     ->>  55) ::
      HNil

  val h4 = h3 + ('name ->> (h3('fname) + "," + h3('lname)))

  println(h4)


}


