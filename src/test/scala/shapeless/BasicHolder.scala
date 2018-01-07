package shapeless

import cc.Spec

class BasicHolder extends Spec {

    case class One(fname: String, lname: String)

  case class Holder(one: One) {

      import record._

      val gen = LabelledGeneric[One]
      val er = gen.to(one)

      def apply(fn: Symbol) = fn match {
        case 'fname => er('fname)
        case 'lname => er('lname)
      }

  }

  "create holder and get the value of a labelled field" in {

    Holder(One("1", "2"))('fname) mustBe "1"
    Holder(One("1", "2"))('lname) mustBe "2"

  }






}


