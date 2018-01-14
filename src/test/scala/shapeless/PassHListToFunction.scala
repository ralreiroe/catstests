package shapeless

import cc.Spec
import shapeless.labelled.KeyTag


/**
  * https://stackoverflow.com/questions/19308143/passing-a-shapeless-extensible-record-to-a-function
  */
class PassHListToFunction extends Spec {

  """constrain function to only work on HLists with a "foo" field""" in {

    import syntax.singleton._

    val w = Witness("foo")

    def fun[L <: HList](xs: L)(implicit sel: ops.record.Selector[L, w.T]) = 41

    fun(("foo" ->> 42) :: HNil) mustBe 41
    fun(("bar" ->> 'a) :: ("foo" ->> 42) :: HNil) mustBe 41
    """fun(("foo" ->> 43) :: HNil)""" must compile
    """fun(("bar" ->> 43) :: HNil)""" mustNot compile


  }
  """constrain function to only work on HLists with a "foo" field II""" in {

    import syntax.singleton._

    def fun[L <: HList](xs: L)(implicit sel: ops.record.Selector[L, Witness.`"foo"`.T]) = 41    //<==== using a literal

    fun(("foo" ->> 42) :: HNil) mustBe 41
    fun(("bar" ->> 'a) :: ("foo" ->> 42) :: HNil) mustBe 41
    """fun(("foo" ->> 43) :: HNil)""" must compile
    """fun(("bar" ->> 43) :: HNil)""" mustNot compile
  }
  """constrain function to only work on HLists with a "foo" field III""" in {

    import syntax.singleton._

    implicit def str2wit(s: String) = Witness(s)

//    implicit val w = "foo"

    def fun[T, L <: HList](xs: L)(s: String)(implicit
                                w: Witness.Aux[T],
                               sel: ops.record.Selector[L,T]) = 41

//    fun(("foo" ->> 42) :: HNil)("foo") mustBe 41
//    fun(("bar" ->> 'a) :: ("foo" ->> 42) :: HNil) mustBe 41
//    """fun(("foo" ->> 43) :: HNil)""" must compile
//    """fun(("bar" ->> 43) :: HNil)""" mustNot compile
  }



  """constrain function to only work on HLists with only "foo" field II""" in {

//    import syntax.singleton._
//
//    val w = Witness("foo")
//
//    def fun(l: Int with KeyTag[w.T, Int] :: HNil) = 41
//
//    fun(("foo" ->> 42) :: HNil) mustBe 41
//    fun(("bar" ->> 'a) :: ("foo" ->> 42) :: HNil) mustBe 41
//    """fun(("foo" ->> 43) :: HNil)""" must compile
//    """fun(("bar" ->> 43) :: HNil)""" mustNot compile


  }


}

