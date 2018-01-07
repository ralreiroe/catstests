package shapeless

import cc.Spec

class MakingALabelledGenericTest extends Spec {

  "https://stackoverflow.com/questions/46248695/generate-shapeless-record-from-generics" in {

    case class KeyValue(key: String, value: Int)
    val kv = KeyValue("key", 1)

    def processCaseClass(p: KeyValue) = {
      val gen = LabelledGeneric[KeyValue]
      gen.to(p)
    }

    println(processCaseClass(KeyValue("k", 5)))


    /**
      * Making a method generic in T makes it clueless as to what T is. If processCaseClass doesn't know what KV is, it stands no chance of analyzing its structure enough to make a LabelledGeneric[KV], producing an implicit resolution error.
      */

    """def processCaseClassWrong[KV <: Product](p: KV) = {
        val gen = LabelledGeneric[KV]
        gen.to(p)
    }""" mustNot compile


    /**
      * By making it an implicit parameter, you shift the responsibility of producing the LabelledGeneric to the caller, where it actually stands a chance of being satisfied, because the caller will know what PK is.
      */


    def processCaseClass2[KV <: Product](p: KV)(implicit gen: LabelledGeneric[KV]) = {
      gen.to(p)
    }

    println(processCaseClass2(KeyValue("k", 5)))



  }

}
