package typeseverywhere

import cc.Spec
import shapeless.{HNil, Poly1}

/**
  *   http://jto.github.io/articles/type-all-the-things/
  */
class TypeAllTheThings extends Spec {

  "You could easily use the default execution context for some DB calls accidentally." in {

    import scala.concurrent.ExecutionContext
//    import play.api.libs.concurrent.Akka

    object Contexts {
      case class DBExeCtx(val underlying: ExecutionContext) extends AnyVal
      case class DefaultExeCtx(val underlying: ExecutionContext) extends AnyVal

      implicit def dbToEC(implicit ec: DBExeCtx) = ec.underlying
      implicit def defaultToEC(implicit ec: DefaultExeCtx) = ec.underlying

      object Implicits {
//        implicit val dbCtx = DBExeCtx(Akka.system.dispatchers.lookup("contexts.db-context"))
//        implicit val defaultCtx = DefaultExeCtx(play.api.libs.concurrent.Execution.defaultContext)
      }
    }

  }


}
