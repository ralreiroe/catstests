package shapeless

import cc.Spec

/**
  * https://stackoverflow.com/questions/25798256/how-to-append-an-element-to-hlist
 */
class HListAppend extends Spec {

  "tuple vs hlist append (latter broken)" in {

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

    val step2 = step1.addElement(nodes => 2.0)    //TestedClass[(Int, Double)] correctly infered in REPL only

    println(step2)
  }

}
