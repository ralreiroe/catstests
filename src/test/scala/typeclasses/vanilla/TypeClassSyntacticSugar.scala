package typeclasses.vanilla

import org.scalatest.{FlatSpec, Matchers}

class TypeClassSyntacticSugar extends FlatSpec with Matchers {

  "" should "" in {

    trait Appendable[A] {
      def append(a: A, b: A): A
    }

    object Appendable {
      implicit val appendableInt = new Appendable[Int] {
        override def append(a: Int, b: Int) = a + b
      }
      implicit val appendableString = new Appendable[String] {
        override def append(a: String, b: String) = a.concat(b)
      }
    }

    implicit val appendableInt2 = new Appendable[Int] {
      override def append(a: Int, b: Int) = a * b
    }

    //(1)
    def appendItems[A](a: A, b: A)(implicit ev: Appendable[A]) = ev.append(a, b)

    println(appendItems(2, 3) )

    def appendItems2[A : Appendable](a: A, b: A) = implicitly[Appendable[A]].append(a, b)   //same as (1) but with syntactic sugar

    println(appendItems2(2, 3) )

    val res: Appendable[Int] = implicitly[Appendable[Int]]

    val res2: Appendable[Int] = Appendable.appendableInt


    println(Range(0, 11))
    println(Range(0, 11).diff(List(3)).map(_.toString * 2) )

  }


}
