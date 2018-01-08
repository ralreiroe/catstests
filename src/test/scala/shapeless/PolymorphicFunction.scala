package shapeless

import cc.Spec

/**
  *   http://kanaka.io/blog/2015/11/09/shapeless-not-a-tutorial-part-1.html
  */
class PolymorphicFunction extends Spec {

  "and here" in {

    object MakeBigger extends Poly1 {

      // When given an Int multiply it by 100
      implicit def intCase = at[Int](_ * 100)

      // When given a String return an upper-cased version of it
      implicit def stringCase = at[String](_.toUpperCase)

    }

    MakeBigger(42) mustBe 4200
    MakeBigger("small") mustBe "SMALL"
    """MakeBigger(true)""" mustNot compile

  }


}
