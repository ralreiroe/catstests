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


}
