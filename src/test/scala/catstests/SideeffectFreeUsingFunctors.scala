package catstests

import cc.Spec

import scala.util.Try

class SideeffectFreeUsingFunctors extends Spec {

    var y: Integer = null
  "Options are good for composition but also good to create side-effect free functions" in {


    //So we could do all kind of mumblejumble inside our functions to try to handle specific side effects (like try-catch statements for handling errors), but that is not extensible or even nice to read
    //https://lunatech.com/blog/WASQJiQAANmjwKxf/side-effects-and-how-to-deal-with-them-the-cool-way-part-1-pure-functions-and-functors

    def nosideeffect(i: Int) = i+1

    def nosideeffect2(oi: Option[Int]) = oi.map(nosideeffect)

    //so a) nosideeffect, a function on Int, can be reused as a function on Option[Int]
    //b) nosideeffect does not have to check for null or catch NullpointerExceptions to stay pure. That's done in the map function


    def getInt: Int = y
    Option.apply(getInt).map(nosideeffect) mustBe null


    Try.apply(1).map(nosideeffect)
    Try.apply({throw new Exception; 1}).map(nosideeffect)





  }
}
