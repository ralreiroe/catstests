package shapeless

import cc.Spec
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Selector

/**
  *
  * https://stackoverflow.com/questions/38946719/how-do-i-best-construct-a-shapeless-record-with-a-default-value
  */
class DefaultVal extends Spec {

  "iii" in {


    trait T
    case object Ti extends T
    case object Ta extends T
    case object To extends T

    import shapeless._ ; import syntax.singleton._

    import record._

    type R = Record.`Ta -> Option[String], Ti -> Option[Int]`.T


//    type R = FieldType[Witness.`'Ta`.T, Option[String]] ::
//      FieldType[Witness.`'Ti`.T, Option[Int]] :: HNil

    val r: R = (Ta ->> Option("plif")) :: (Ti ->> Option(4)) :: HNil

    import shapeless._
    import shapeless.ops.record.Selector

    trait DefaultSelector[R <: HList, K] {
      type Out
      def apply(r: R): Out
    }

    sealed trait LowPriorityDefaultSelector {
      type Aux[R <: HList, K, V] = DefaultSelector[R, K] { type Out = V }

      case class Impl[R <: HList, K, V](get: R => V) extends DefaultSelector[R, K] {
        type Out = V
        def apply(r: R): Out = get(r)
      }

      implicit def default[R <: HList, K, V](
                                              implicit ev: Option[Nothing] =:= V  // tricking Scala's implicit resolution
                                            ): Aux[R, K, V] =
        Impl[R, K, V](Function.const(None))
    }

    object DefaultSelector extends LowPriorityDefaultSelector {
      implicit def existing[R <: HList, K, V](
                                               implicit sel: Selector.Aux[R, K, V]
                                             ): Aux[R, K, V] =
        Impl[R, K, V](sel.apply)
    }


    def get[K, V](k: K)(implicit sel: DefaultSelector[R, K]): sel.Out = sel(r)



    println(get(Ti))
  }

}
