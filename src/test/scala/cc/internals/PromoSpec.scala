package cc
package internals

import Product._
import spire.implicits._

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

class PromoSpec extends Spec {

  "buy one, get one free on Apples" in {
    Promo.discount1(Map[Product, Count](apple -> 2, orange -> 0)) mustBe apple.price withClue "one apple for free"
    Promo.discount1(Map[Product, Count](apple -> 1, orange -> 1)) mustBe apple.price withClue "one apple for free - even if second fruit is orange"
    Promo.discount1(Map[Product, Count](apple -> 2, orange -> 5)) mustBe (apple.price * 2) withClue "two apples for free"
    Promo.discount1(Map[Product, Count](apple -> 10, orange -> 5)) mustBe (apple.price * 7) withClue "only one apple for free (two apples)"
    Promo.discount1(Map[Product, Count](apple -> 10, orange -> 0)) mustBe (apple.price * 5) withClue "half of apples for free"
    Promo.discount1(Map[Product, Count](apple -> 11, orange -> 0)) mustBe (apple.price * 5) withClue "half of apples for free - even if there is odd number of them"
    Promo.discount1(Map[Product, Count](apple -> 0, orange -> 1)) mustBe 0 withClue "no apples at all"
  }

  "3 for the price of 2 on Oranges" in {
    Promo.discount2(Map[Product, Count](apple -> 0, orange -> 3)) mustBe orange.price withClue "one orange for free"
    Promo.discount2(Map[Product, Count](apple -> 10, orange -> 3)) mustBe orange.price withClue "one orange for free - even if there are apples"
    Promo.discount2(Map[Product, Count](apple -> 10, orange -> 4)) mustBe orange.price withClue "still one orange for free - you need two more to get second one discounted"
    Promo.discount2(Map[Product, Count](apple -> 1, orange -> 10)) mustBe (orange.price * 3) withClue "for 10 oranges 3 goes for free"
    Promo.discount2(Map[Product, Count](apple -> 10, orange -> 2)) mustBe 0 withClue "no discount for oranges"
  }

  "" in {

    val sfc = StatefulCheckout.apply
    sfc.addItem(Product.apple)
    sfc.totalCost() mustBe apple.price
    sfc.addItem(Product.apple)
    sfc.totalCost() mustBe apple.price
    sfc.addItem(Product.apple)
    sfc.totalCost() mustBe (apple.price * 2)
  }



object StatefulCheckout {

  def apply: StatefulCheckout = new StatefulCheckout {
    var seenSofar = new ArrayBuffer[Product]

    override def addItem(product: Product): Unit = seenSofar.+=(product)

    override def totalCost(): Price = CheckoutCounter.computeCost(seenSofar.head, seenSofar.tail:_*)
  }
}


  trait StatefulCheckout {
    def addItem(product: Product)
    def totalCost(): Price
  }

}