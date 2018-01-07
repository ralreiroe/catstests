package shapeless

import cc.Spec

class BasicHolder extends Spec {

  "create holder and get the value of a labelled field" in {

    case class One(fname: String, lname: String)

    case class Holder(one: One) {

      import record._

      val gen = LabelledGeneric[One]
      val er = gen.to(one)

      def apply(fn: Symbol) = er('fname)
    }


    Holder(One("1", "2"))('fname) mustBe 1

  }




}


