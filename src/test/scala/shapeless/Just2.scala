package shapeless

import cc.Spec

class Just2 extends Spec {

  def singletonValue[K <: Symbol](implicit ev: Witness.Aux[K]): ev.T = ev.value

  case class Holder() {

    import record._

    def just2(arg: Witness.`2`.T) = arg
    def justfname(arg: Witness.`'fname`.T) = arg

  }


  "call method that only accepts Integer 2, fails to compile otherwise" in {

    //Types for All Literals: http://enear.github.io/2016/09/27/bits-of-shapeless-2/


    Holder().just2(2)
    """Holder().just2(3)""" mustNot compile
    Holder().justfname(singletonValue[Witness.`'fname`.T])


  }



}


