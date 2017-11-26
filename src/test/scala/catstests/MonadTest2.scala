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

    val res: List[DateFormatConfigProblem] = exchangeIds.flatMap {
      case exId => {
        val fids = getFormatIds(exId)

        val fileformats: List[String] = fids.map {
          case fid => getExchangeFormat(exId, fid)
        }

        val triedStrings: List[Try[String]] = fileformats.flatMap {
          ff => validateDatePatterns(ff)
        }

        val res: List[Option[DateFormatConfigProblem]] = triedStrings.map {
          case t => t match {
            case Failure(ex) => Some(DateFormatConfigProblem(exId, ex.getMessage))
            case successful => None
          }
        }

        res collect {
          case Some(x) => x
         }
      }
    }

    println(res)




  }


}
