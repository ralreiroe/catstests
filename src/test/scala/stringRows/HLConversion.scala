package stringRows

import shapeless.{::, HList, HNil}

trait HLConversion[I, O] {
  def apply(source: I): Option[O]
}

object HLConversion {

  implicit def empty =
    new HLConversion[HNil, HNil] {
      def apply(source: HNil) = Some(HNil)
    }

  implicit def nonEmpty[STail <: HList, HHead, HTail <: HList](implicit
                                                               convertSingle: StringConversion[HHead],
                                                               convertTail: HLConversion[STail, HTail]
                                                              )

  = new HLConversion[String :: STail, HHead :: HTail] { //<=================================================================

    def apply(from: String :: STail) = from match {
      case x :: xs => for {
        t <- convertSingle(x)
        ts <- convertTail(xs)
      } yield t :: ts
    }
  }

}