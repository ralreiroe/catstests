package shapeless

import cc.Spec

/**
  *   http://kanaka.io/blog/2015/11/09/shapeless-not-a-tutorial-part-1.html
  */
class PolymorphicFunction extends Spec {

  "simple example" in {

    object MakeBigger extends Poly1 {

      implicit def intCase = at[Int](_ * 100)       // When given an Int multiply it by 100
      implicit def stringCase = at[String](_.toUpperCase)         // When given a String return an upper-cased version of it
    }

    MakeBigger(42) mustBe 4200
    MakeBigger("small") mustBe "SMALL"
    """MakeBigger(true)""" mustNot compile

    val demo = 42 :: "small" :: HNil
    demo.map(MakeBigger) mustBe 4200 :: "SMALL" :: HNil

  }

  "map over hlist with polymorphic function" in {

    // http://jto.github.io/articles/getting-started-with-shapeless/

    case class User(name: String)
    val demo = 42 :: "Hello" :: User("Julien") :: HNil

    import cats._

    implicit val stringShow = Show.fromToString[String]
    implicit val intShow = Show.fromToString[Int]
    implicit val userShow = Show.fromToString[User]

    object show extends Poly1 {
      implicit def showa[A](implicit s: Show[A]) = at[A]{ a => "Showing " + s.show(a) }
    }

    demo.map(show) mustBe "Showing 42" :: "Showing Hello" :: "Showing User(Julien)" :: HNil
  }

  "cesium output" in {

    val removeCountryCode: String=>String = str=>str.substring(Math.min(str.size,2),str.size)
    val takeEnd: String=>String = { str => val aftrlastfwslash = ".*/([^/]+)$".r; str match { case aftrlastfwslash(res) => res; case _ => str } }

    object transformer extends Poly1 {
      implicit def sc = at[ShortCode](_.v)
      implicit def lc = at[LongCode](lc=>LongCode(removeCountryCode(lc.v)))
      implicit def dcc = at[DomCountryCode](cc=>CountryCode(takeEnd(cc.v)))
      implicit def lcc = at[LeiCountryCode](cc=>CountryCode(takeEnd(cc.v)))
      implicit def cc = at[(DomCountryCode,LeiCountryCode)](t=>transformer(t._1))
    }
    val l1 = List("42", "GB10098765", "a/b/GB", "a/b/DE")
    val co0 = ShortCode(l1(0)) :: LongCode(l1(1)) :: (DomCountryCode(l1(2)), LeiCountryCode(l1(3))) :: HNil
    co0.map(transformer) mustBe "42" :: LongCode("10098765") :: CountryCode("GB") :: HNil
    val co1 = ShortCode("42") :: LongCode("GB10098765") :: DomCountryCode("a/b/GB") :: LeiCountryCode("a/b/DE") :: HNil
    co1.map(transformer) mustBe "42" :: LongCode("10098765") :: CountryCode("GB") :: CountryCode("DE") :: HNil
    val co2 = ShortCode("42") :: LongCode("GB10098765") :: (DomCountryCode("a/b/GB"), LeiCountryCode("a/b/DE")) :: HNil
    co2.map(transformer) mustBe "42" :: LongCode("10098765") :: CountryCode("GB") :: HNil
  }

}
case class ShortCode(v: String) extends AnyVal
case class LongCode(v: String) extends AnyVal
case class DomCountryCode(v: String) extends AnyVal
case class LeiCountryCode(v: String) extends AnyVal
case class CountryCode(v: String) extends AnyVal
