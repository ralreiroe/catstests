package shapeless

import cc.Spec

/**
  * https://stackoverflow.com/questions/25798256/how-to-append-an-element-to-hlist
 */
class HListAreArityAbstractions extends Spec {

  "flattening" in {


    import shapeless._
    import ops.tuple.FlatMapper
    import syntax.std.tuple._
    import test._

    trait LowPriorityFlatten extends Poly1 {
      implicit def default[T] = at[T](Tuple1(_))
    }
    object flatten extends LowPriorityFlatten {
      implicit def caseTuple[P <: Product](implicit lfm: Lazy[FlatMapper[P, flatten.type]]) =
        at[P](lfm.value(_))
    }

    val t1 = (1, ((2, 3), 4))
    val f1 = flatten(t1)     // Inferred type is (Int, Int, Int, Int)

    println(s"$t1 -> ${f1/*.tupled*/}")

  }

  "tuple vs hlist append (latter broken)" in {

    /**
      *
      * The tuple vs list naming isn't significant. HLists could be named as HTuples. The difference is that in Scala+Haskell, you can do this with a tuple (using Scala syntax):

def append2[A,B,C](in: (A,B), v: C) : (A,B,C) = (in._1, in._2, v)
to take an input tuple of exactly two elements of any type, append a third element, and return a fully typed tuple with exactly three elements. But while this is completely generic over types, it has to explicitly specify the input/output
    *lengths*.

What a Haskell style HList lets you do is make this generic over length, so you can append to any length of tuple/list and get back a fully statically typed tuple/list. This benefit also applies to homogeneously typed collections where you can append an int to a list of exactly n ints and get back a list that is statically typed to have exactly (n+1) ints without explicitly specifying n.
      */

    def append2[A,B,C](in: (A,B), v: C) : (A,B,C) = (in._1, in._2, v)

    println(append2(("1", 2), 3.0))

    def appendH[C](in: HList, v: C) : HList = in :: (v :: HNil)

    println(appendH(1 :: "hello" :: HNil, true))

  }

  "hlist append" in {

    import shapeless._, ops.tuple.Prepend

    class TestedClass[HL](nodes: HL) {
      def addElement[T, OUT](clause: HL => T)
                            (implicit prepend: Prepend.Aux[HL, Tuple1[T], OUT]) = {
        new TestedClass[OUT](prepend(nodes, Tuple1(clause(nodes))))
      }
    }

    val step1: TestedClass[Tuple1[Int]] = new TestedClass[Tuple1[Int]](Tuple1(1))

    println(step1)

    val step2: TestedClass[(Int, Double)] = step1.addElement(nodes => 2.0)

    println(step2)
  }

}
