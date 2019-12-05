import cc.Spec

/**
  * https://stackoverflow.com/questions/31818787/pure-functional-random-number-generator-state-monad
  */
class FunctionalRandom extends Spec {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
          ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
      }
    }
  }

  """That random generator RNG is pure functional, for the same inputs you get always the same outputs. The non-pure-functional part is left for the user of that API (you).
    |
    |To use the RNG in a pure-functional way you have to initialize it always with the same initial value, but then you will always get the same sequence of numbers, which is not so useful.
    |
    |Otherwise, you will have to rely the initialization of RNG to an external system (usually the wall-clock time) and so introducing side effects (bye pure functional).""" in {

    val rng0 = RNG.simple(System.currentTimeMillis)

    val (rnd1, rng1) = rng0.nextInt
    val (rnd2, rng2) = rng1.nextInt
    val (rnd3, rng3) = rng2.nextInt

    println(rnd1, rnd2, rnd3)

  }


}
