package autoconversion

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil}

trait Copier[T] {
  def apply(elements: T): T
}

object Copier {

  implicit def empty: Copier[HNil] = new Copier[HNil] {
    def apply(elements: HNil): HNil = HNil
  }

  implicit def nonEmpty[T1, T2 <: HList](implicit copier: Copier[T2]) =
    new Copier[T1 :: T2] {                                                                  //pattern match <====================
      def apply(elements: T1 :: T2): T1 :: T2 = elements.head :: copier(elements.tail)
    }
}

object HListCopy {

  def copyHList[T](source: T)(implicit copier: Copier[T]): T = copier.apply(source)
}


object ConvertUsingHList extends App {

  val hlist: Int::String::HNil = 1 :: "quux" :: HNil
  val copy = HListCopy.copyHList(hlist)   //(1) copyHList requires a Copier[Int::String::HNil] which only nonEmpty provides (line 17)
  println(copy)
  //Again in line 17, the type parameters are replaced by the real types: Int replaces T1, String::HNil replaces T2
  val copyWithOneImplicitInserted = HListCopy.copyHList[Int :: String :: HNil](hlist)(
    Copier.nonEmpty[Int, String :: HNil]
  )
  val copyWithTwoImplicitsInserted = HListCopy.copyHList[Int :: String :: HNil](hlist)(
    Copier.nonEmpty[Int, String :: HNil](
      Copier.nonEmpty[String, HNil]
    )
  )
  val copyWithThreeImplicitsInserted = HListCopy.copyHList[Int :: String :: HNil](hlist)(
    Copier.nonEmpty[Int, String :: HNil](
      Copier.nonEmpty[String, HNil](
        Copier.empty
      )
    )
  )


  //illustration how the compiler insert calls to the implicit defs HListCopier.nonEmptyHListCopier and HListCopier.hNilCopier f
  // or the implicit arg required by copyHList
  val copy3 = HListCopy.copyHList(hlist)(
      Copier.nonEmpty(
        Copier.nonEmpty(
          Copier.empty
      )
    )
  )

  println("copy: "+copy)
  println("copyWithThreeImplicitsInserted: " +copyWithThreeImplicitsInserted)


  import shapeless._ ; import syntax.singleton._

  type BookRec =
    FieldType[Witness.`"author"`.T, String] ::
      FieldType[Witness.`"title"`.T, String] ::
      FieldType[Witness.`"id"`.T, Int] ::
      FieldType[Witness.`"price"`.T, Double] :: HNil

  val book: BookRec =
    ("author" ->> "Benjamin Pierce") ::
      ("title"  ->> "Types and Programming Languages") ::
      ("id"     ->>  262162091) ::
      ("price"  ->>  44.11) ::
      HNil
  val bookcopy: BookRec = HListCopy.copyHList(book)

  println(book)
  println(bookcopy)




}




