package shapeless

import cc.Spec
import ops.hlist.Tupler

/**
  *
  * https://gist.github.com/milessabin/fbd9da3361611b91da17
  */
trait TupleGeneric[C <: Product] extends Serializable {
  type Repr <: Product

  def to(t : C) : Repr

  def from(r : Repr) : C
}

object TupleGeneric {
  type Aux[C <: Product, R] = TupleGeneric[C] { type Repr = R }

  def apply[C <: Product](implicit tgc: TupleGeneric[C]): Aux[C, tgc.Repr] = tgc

  implicit def mkTG[C <: Product, L <: HList, R <: Product]
  (implicit cGen: Generic.Aux[C, L], tup: Tupler.Aux[L, R], tGen: Generic.Aux[R, L]): Aux[C, R] =
    new TupleGeneric[C] {
      type Repr = R

      def to(t : C) : Repr = cGen.to(t).tupled

      def from(r : Repr) : C = cGen.from(tGen.to(r))
    }
}

class CaseClassToTuple extends Spec {

  "convert example" in {
    case class Person(name: String, age: Int)

    val gen = TupleGeneric[Person]

    val mary = Person("Mary", 37)
    val res0: (String, Int) = gen.to(mary)
    res0 mustBe ("Mary", 37)

    val res1: Person = gen.from(("Fred", 23))
    res1 mustBe Person("Fred", 23)
  }

  "2" in {
    case class One(fname: String, lname: String)
    case class Two(name: String)

    class Holder(one: One) {
//      def apply(a: Symbol) = one.
    }


  }

}
