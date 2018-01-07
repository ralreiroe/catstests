package shapeless

import cc.Spec

class AccessRecordByIndex extends Spec {

  val l = 1 :: "hello" :: true :: HNil
  // l: shapeless.::[Int,shapeless.::[String,shapeless.::[Boolean,shapeless.HNil]]] = 1 :: hello :: true :: HNil

  println(l.head)
  // res7: Int = 1

  println(l.tail)
  // res8: shapeless.::[String,shapeless.::[Boolean,shapeless.HNil]] = hello :: true :: HNil

  println(l(1))
  // res9: String = hello

  println(l(2))

  case class One(fname: String, lname: String)

  val one = One("1", "2")

  val lgen = LabelledGeneric[One]
  val gen = Generic[One]
  val r: ::[String, ::[String, HNil]] = gen.to(one)
  var er = lgen.to(one)

  println(r.head)
  //
  //  r(1)    //does not work


}


