package shapeless

import cc.Spec

class CaseClassConversion extends Spec {

  "https://stackoverflow.com/questions/29242873/shapeless-turn-a-case-class-into-another-with-fields-in-different-order using Align" in {


    import shapeless._, ops.hlist.Align

    class SameFieldsConverter[T] {
      def apply[S, SR <: HList, TR <: HList](s: S)(implicit
                                                   genS: LabelledGeneric.Aux[S, SR],
                                                   genT: LabelledGeneric.Aux[T, TR],
                                                   align: Align[SR, TR]
      ) = genT.from(align(genS.to(s)))
    }

    def convertTo[T] = new SameFieldsConverter[T]


    case class A(foo: Int, bar: Int)
    case class B(bar: Int, foo: Int)

    convertTo[B](A(1,12)) mustBe B(12,1)




  }

}
