package catstests

import cc.Spec

import scala.util.Try

class SideeffectFreeUsingFunctors extends Spec {

    var y: Integer = null
  "Options are good for composition but also good to create side-effect free functions" in {


    //So we could do all kind of mumblejumble inside our functions to try to handle specific side effects (like try-catch statements for handling errors), but that is not extensible or even nice to read
    //https://lunatech.com/blog/WASQJiQAANmjwKxf/side-effects-and-how-to-deal-with-them-the-cool-way-part-1-pure-functions-and-functors

    def nosideeffect(i: Int) = i+1

    Some(5).map(nosideeffect)


    def getInt(x: Int): Int = x match {
      case 5 => 5
      case _ => y
    }
    Option.apply(getInt(4)).map(nosideeffect)


    Try.apply(1).map(nosideeffect)
    println( Try.apply({throw new Exception; 1}).map(nosideeffect) )





  }
}
