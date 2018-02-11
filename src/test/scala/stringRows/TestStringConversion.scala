package stringRows

import cc.Spec
import shapeless.{::, HNil}

class TestStringConversion extends Spec {

  type S = String :: String :: HNil
  type H = Double :: Int    :: HNil

  "a" in {

    val stringsHL: S = "1234.0" :: "1234" :: HNil

    When("we summon an HLConversion[S, H]")
    var x: Option[H] = null
    x = implicitly[HLConversion[S, H]].apply(stringsHL)
    println(x)
    x mustBe Some(1234.0 :: 1234 :: HNil)

    Then("the compiler inserts these calls to make one")
    x = HLConversion.nonEmpty(
      StringConversion.convertToDouble,
      HLConversion.nonEmpty(
        StringConversion.convertToInt,
        HLConversion.empty
      )
    ).apply(stringsHL)
    x mustBe Some(1234.0 :: 1234 :: HNil)




  }
}