package cc.internals

import cc.Spec

class HashMapOwnImplementationTest extends Spec {


  class MyHashMap[K,V](size: Int) {

    case class Entry[K,V] (k: K, var v: V, var n: Option[Entry[K,V]] = None)

    val arr: Array[Entry[K,V]] = Array.ofDim[Entry[K,V]](size)

    def put(k: K, v: V): Unit = {

      val slot: Int = k.hashCode % arr.length

      if (arr(slot)==null) {
        arr(slot) = Entry(k,v)
        return
      }

      var e: Entry[K, V] = arr(slot)

      if (e.k==k) {
        e.v = v
        return
      }

      while (e.n.isDefined) {
        e = e.n.get
        if (e.k == k) {
          e.v = v
          return
        }
      }

      e.n = Some(Entry(k,v))
    }

    def get(k: K): Option[V] = {
      val slot = k.hashCode % arr.length

      if (arr(slot)==null) None else {

        var e: Entry[K, V] = arr(slot)

        if (e.k==k) return Some(e.v)

        while(e.n.isDefined) {
          e = e.n.get
          if (e.k==k) return Some(e.v)
        }

        None
      }
    }

  }

  "sldjflsk" in {

    case class Person(fn: String, ln: String, yob: Int)
    val myhm = new MyHashMap[String, Person](10)

    println(myhm.get("123"))      //Test None

    myhm.put("123", Person("Ralf", "Oenning", 1976))

    println(myhm.get("123"))    //Test Single
    println(myhm.arr.toList)

    myhm.put("123", Person("Ralf", "Oenning", 1977))    //Test replacing
    println(myhm.arr.toList)
    myhm.put("132", Person("Bernardita", "Jimenez", 1977))    //Test adding different key to same slot
    println(myhm.arr.toList)

    println(myhm.get("123"))
    println(myhm.get("132"))


    println("123".hashCode % 10)
    println("124".hashCode % 10)
    println("125".hashCode % 10)
    println("132".hashCode % 10)
    println("133".hashCode % 10)


  }
}
