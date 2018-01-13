package shapeless

import cc.Spec

/**
  */
class DefaultVal0 extends Spec {

  "iii" in {

    trait T
    case object Ti extends T
    case object Ta extends T
    case object To extends T

    import record._
    import syntax.singleton._

    type R = Record.`Ta -> Option[String], Ti -> Option[Int]`.T
    val r: R = (Ta ->> Option("plif")) :: (Ti ->> Option(4)) :: HNil

    import shapeless.ops.record.Selector

    def get[K, V](k: K)(implicit sel: Selector[R, K]): sel.Out = sel(r)

    println(get(Ti))
    println(get(Ta))
//    println(get(To))

  }

}
