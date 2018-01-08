package shapeless

import cc.Spec

/**
  *   * http://gigiigig.github.io/posts/2015/09/13/aux-pattern.html
  */
class AuxTechnique extends Spec {

  trait Foo[A] {
    type B            //<=====
    def value: B
  }

  def fis = new Foo[Int] {
    type B = String
    val value = "Foo"
  }

  def fsb = new Foo[String] {
    type B = Boolean
    val value = false
  }

  "resolve f.B works here" in {

    def valueOf[T](f: Foo[T]): f.B = f.value

    valueOf(fis) mustBe "Foo"
    valueOf(fsb) mustBe false

  }

  "and here" in {

    import cats.Monoid

    import cats.implicits._

    def valueOf[T](f: Foo[T])(implicit m: Monoid[f.B]): f.B = m.empty

    valueOf(fis) mustBe ""

  }

  """but not here
    |
    |Scala compiler cannot resolve Monoid[f.B] because it is in the same section as f
    |
    |got the following type error: "illegal dependent method type: parameter may only be referenced in a subsequent parameter section" """.stripMargin in {

    """
      import cats.Monoid
      def valueOf[T](t: T)(implicit f: Foo[T], m: Monoid[f.B]): f.B = m.empty
    """ mustNot compile

  }

  "iii" in {

//    object Foo {
//
//      type Aux[A0, B0] = Foo[A0] { type B = B0 }
//
//      implicit def fbi = new Foo[Boolean] {
//        type B = Int
//        val value = 3
//      }
//    }
//
//
//    import cats.Monoid
//    import cats.implicits._
//
//    def valueOf[T, R](t: T)(implicit f: Foo.Aux[T, R], m: Monoid[R]): R = m.empty
//
//    println(valueOf(true))


  }

}
