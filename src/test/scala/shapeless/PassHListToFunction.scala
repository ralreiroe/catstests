package shapeless

import cc.Spec


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


}

