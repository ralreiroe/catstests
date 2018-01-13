package autoconversion

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil}

trait HListCopier[Elements] {

  def apply(elements: Elements): Elements
}

object HListCopier {

  implicit def hNilCopier: HListCopier[HNil] = new HListCopier[HNil] {
    def apply(elements: HNil): HNil = HNil
  }

  implicit def nonEmptyHListCopier[A, Tail <: HList](implicit hlistCopier: HListCopier[Tail]) =
    new HListCopier[A :: Tail] {
      def apply(elements: A :: Tail): A :: Tail = elements.head :: hlistCopier(elements.tail)
    }
}

object HListCopy {

  def copyHList[Source <: HList](source: Source)(implicit hListCopier: HListCopier[Source]): Source =
    hListCopier(source)
}


object ConvertUsingHList extends App {

  val hlist = 1 :: "quux" :: HNil
  val copy = HListCopy.copyHList(hlist)
  println(copy)


  //illustration how the compiler insert calls to the implicit defs HListCopier.nonEmptyHListCopier and HListCopier.hNilCopier f
  // or the implicit arg required by copyHList
  val copy2 = HListCopy.copyHList(hlist)(
      HListCopier.nonEmptyHListCopier(
        HListCopier.nonEmptyHListCopier(
          HListCopier.hNilCopier
      )
    )
  )

  println(copy2)


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




