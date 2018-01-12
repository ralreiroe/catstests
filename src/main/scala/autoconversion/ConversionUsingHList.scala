package autoconversion

import shapeless.{HList, ::, HNil}

trait HListCopier[Elements] {

  def apply(elements: Elements): Elements
}

object HListCopier {


  implicit def hNilCopier: HListCopier[HNil] = new HListCopier[HNil] {
    def apply(elements: HNil): HNil = HNil
  }


  implicit def nonEmptyHListCopier[A, Rest <: HList](implicit restCopier: HListCopier[Rest]): HListCopier[A :: Rest] =
    new HListCopier[A :: Rest] {
      def apply(elements: A :: Rest): A :: Rest = elements.head :: restCopier(elements.tail)
    }
}

object HListCopy {

  def copyHList[Source <: HList](source: Source)(implicit hListCopier: HListCopier[Source]): Source =
    hListCopier(source)
}


object ConvertUsingHList extends App {

  val hlist = 1 :: "quux" :: HNil // Int :: String :: HNil
  val copy = HListCopy.copyHList(hlist)

  println(copy)

  import shapeless._ ; import syntax.singleton._
  val book: HList =
    ("author" ->> "Benjamin Pierce") ::
      ("title"  ->> "Types and Programming Languages") ::
      ("id"     ->>  262162091) ::
      ("price"  ->>  44.11) ::
      HNil
  val bookcopy = HListCopy.copyHList(book)

  println(bookcopy)




}




