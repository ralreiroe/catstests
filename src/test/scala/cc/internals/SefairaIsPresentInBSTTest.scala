package cc.internals

import cc.Spec

class SefairaIsPresentInBSTTest extends Spec {

  case class Node(l: Option[Node]=Option.empty[Node], r: Option[Node]=Option.empty[Node], v: Int)


  def isPresentIf(r: Option[Node], x: Int): Boolean = {

    if (r.isEmpty) return false

    if (r.get.v==x) return true

    if (r.get.v<x) return isPresentIf(r.get.r, x)

    return isPresentIf(r.get.l, x)
  }

  def isPresentMatch(r: Option[Node], x: Int): Boolean = {
    r match {
      case None => false
      case Some(Node(_, _, v)) if (v==x) => true
      case Some(Node(lo, ro, v)) if (x>v) => isPresentMatch(ro, x)
      case Some(Node(lo, _, _)) => isPresentMatch(lo, x)
    }
  }

  "slkdflks" in {

    val l10 = Node(Some(Node(None, None, 8)), Some(Node(None, None, 12)), 10)
    val r30 = Node(Some(Node(None, None, 25)), Some(Node(None, None, 40)), 30)

    val root =
      Node(
        Some(
          Node(Some(Node(None, None, 8)), Some(Node(None, None, 12)), 10)),
        Some(
          Node(Some(Node(None, None, 25)), Some(Node(None, None, 40)), 30)),
        20)

    List(30,10,12,15).map(i => isPresentIf(Some(root), i)) mustBe List(true, true, true, false)
    List(79,10,20,30,40).map(i => isPresentIf(Some(root), i)) mustBe List(false, true, true, true, true)
    List(30,10,12,15).map(i => isPresentMatch(Some(root), i)) mustBe List(true, true, true, false)
    List(79,10,20,30,40).map(i => isPresentMatch(Some(root), i)) mustBe List(false, true, true, true, true)
  }

}
