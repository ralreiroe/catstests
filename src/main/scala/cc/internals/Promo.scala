package cc
package internals

import Product._

object Promo {

  /**
    * This computes discount for business offer "buy one, get one free on Apples". See test case in order to understand
    * how it works.
    */
  def discount1(products: Map[Product, Count]): Price = {
    val apples = products(apple)
    val oranges = products(orange)
    val totalProducts = apples + oranges
    val atMostCanBeForFree = totalProducts/2 //flor division
    if(oranges < apples) atMostCanBeForFree * apple.price else apples * apple.price
  }

  /**
    * This computes discount for business offer "3 for the price of 2 on Oranges". See test case in order to understand
    * how it works.
    */
  def discount2(products: Map[Product, Count]): Price = {
    val oranges = products(orange)
    val howManyOrangesForFree = oranges / 3
    howManyOrangesForFree * orange.price
  }

}
