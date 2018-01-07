package shapeless

import cc.Spec

class BasicHolder extends Spec {

  case class One(fname: String, lname: String)

  case class Holder(one: One) {

    import record._

    val gen = LabelledGeneric[One]
    var er = gen.to(one)

    def apply(fn: Symbol) = fn match {
      case 'fname => er('fname)
      case 'lname => er('lname)
    }

    def update(fn: Symbol, ln: String) = {

      fn match {
        case 'fname => er = er.updated('fname, ln)
      }

    }
  }

  val one = One("1", "2")
  val h = Holder(one)

  "create holder and get the value of a labelled field" in {

    h('fname) mustBe "1"
    h('lname) mustBe "2"

  }

  "swap" in {

    val ln = h('lname)
    h update ('fname, ln)
    println(h('fname))

  }


}


