package shapeless

import cats.Monoid
import cc.Spec
import cats.implicits._
import shapeless.ops.hlist.Length



trait Foo[A] {
  type B
  def value: B
}

object Foo {
  type Aux[A0, B0] = Foo[A0] { type B = B0 }

  implicit def fi = new Foo[Int] {
    type B = String
    val value = "FooValue"
  }
  implicit def fs = new Foo[String] {
    type B = Boolean
    val value = false
  }

}


/**
  * http://gigiigig.github.io/posts/2015/09/13/aux-pattern.html
  *
  */
class AuxTechnique2 extends Spec {

  "lll" in {


    def ciao[T, R](t: T)
                  (implicit f: Foo.Aux[T, R],
                   m: Monoid[R]): R = f.value           //<====== now we are not resolving f.B but R
    val res = ciao(2)
    res mustBe "FooValue"


  }

  "real ex" in {

    def length[T, R <: HList](t: T)
                             (implicit
                              g: Generic.Aux[T, R],
                              l: Length[R]) = l()


    case class Foo(i: Int, s: String, b: Boolean)
    val foo = Foo(1, "", false)

    val res = length(foo)       //Succ[Succ[Succ[_0]]]

    Nat.toInt(res) mustBe 3
  }




}

