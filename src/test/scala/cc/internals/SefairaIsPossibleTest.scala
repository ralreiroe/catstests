package cc.internals

import cc.Spec

class SefairaIsPossibleTest extends Spec {

  def op1(a: Int, b: Int) = (a+b, b)

  def isPossible(a: Int, b: Int, c: Int, d: Int): Boolean = {

    println(a+"|"+b+"|"+c+"|"+d)

//    if (a==c && b==d) true else {
//
//      if (a>c || b>d) false else {
//
//        isPossible(a + b, b, c, d) || isPossible(a, a + b, c, d)
//      }
//    }

    (a,b) match {
      case (x,y) if (x==c && y==d) => true
      case (x,y) if (x>c || y>d) => false
      case _ => isPossible(a + b, b, c, d) || isPossible(a, a + b, c, d)
    }

  }

  "" in {

//    isPossible(1,4,5,4) mustBe true
//    isPossible(5,4,5,9) mustBe true
//    isPossible(1,4,5,14) mustBe true
//    isPossible(1,4,5,9) mustBe true
//
//    isPossible(5,2,3,6) mustBe false
    isPossible(1,2,3,6) mustBe false
    isPossible(10,20,800,1000) mustBe false
  }

}
