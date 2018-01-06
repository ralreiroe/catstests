package shapeless

import cc.Spec
import ops.hlist.Tupler

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

  "" in {

    case class Person(name: String, age: Int)

    val gen = TupleGeneric[Person]

    val mary = Person("Mary", 37)

    val res0: (String, Int) = gen.to(mary)

    println(res0)

    val res1: Person = gen.from(("Fred", 23))

    println(res1)

  }

}
