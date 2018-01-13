package shapeless

import cc.Spec
import shapeless.labelled.FieldType

/**
  */
class DefaultVal1 extends Spec {

  "iii" in {

    import record._
    import syntax.singleton._

//    type R = Record.`'fname -> String, 'lname -> String`.T

    type R = FieldType[Witness.`'fname`.T, String] :: HNil
    val r: R = ('fname ->> "1") :: HNil

    import shapeless.ops.record.Selector

    def get[K, V](k: K)(implicit sel: Selector[R, K]): sel.Out = sel(r)

    def singletonValue[K <: Symbol](implicit ev: Witness.Aux[K]): ev.T = ev.value

    println(get(singletonValue[Witness.`'fname`.T]))

  }

}
