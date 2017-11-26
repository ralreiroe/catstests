package catstests

import cc.Spec

import scala.util.{Failure, Try}

class MonadTest2 extends Spec {

  "b" in {

    val exchangeIds = List("oslo", "ice", "lse")
    def getFormatIds(exchId: String) = List("employeeIDM", "algoE")
    def getExchangeFormat(exId: String, fid: String) = "shortcode, longcode, datetimeyyyyMMdd"
    def validateDatePatterns(format: String): List[Try[String]] = List(Failure(new IllegalArgumentException))
    case class DateFormatConfigProblem(exId: String, reason: String)

    exchangeIds.flatMap {
      case exId => getFormatIds(exId)
    }

    exchangeIds.flatMap {
      case exId => {
        val fids = getFormatIds(exId)


        fids
      }
    }

    val res2 =
    (for {
      exId <- exchangeIds
      fid <- getFormatIds(exId)
      ff = getExchangeFormat(exId, fid)
      triedString <- validateDatePatterns(ff)
    } yield {
      triedString match {
        case Failure(ex) => Some(DateFormatConfigProblem(exId, ex.getMessage))
        case successful => None
      }
    }) collect { case Some(x) => x}

    println(res2)




  }


}
