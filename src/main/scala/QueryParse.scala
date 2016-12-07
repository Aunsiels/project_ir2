/**
  * @author Michael
  *
  */

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.Map


case class QueryParse(fname: String, 
              options: TipsterOptions = TipsterOptions(),
              includeSynonymsInQuery: Boolean = false) {

  var queries = Map[String, List[String]]()

  /* Parse entries that look like this. Note that title can extend lines until the next <desc>
  <num> Number: 090

  <dom> Domain: International Economics

  <title> Topic: Data on Proven Reserves of Oil & Natural Gas
  Producers
  */

  private val patNumber = "<num>\\s+Number:\\s+(\\d+)\\s*".r
  private val patTitle = "<title>\\s+Topic:\\s+(.+)\\s*".r
  private val patCont = "\\s*(\\S.*)\\s*".r
  private val patDesc = "<desc>(.+)".r

  private var tempNum = ""
  private var tempTit = ""

  Source.fromFile(fname).getLines()
    .foreach {
      case patNumber(num) => tempNum = num
      case patTitle(tit) => tempTit = tit
      case patDesc(d) =>
        if(includeSynonymsInQuery) {
          tempTit = Wordnet.expandTermBySynonyms(tempTit)
        }
        queries += tempNum -> TipsterParseSmart.tokenize(tempTit, options)
        tempTit = ""
      case patCont(x) => if (!tempTit.isEmpty) tempTit ++= " " + x
      case _ =>
    }
  //println(queries.toSeq.sortWith(_._1 < _._1))

}

object QueryParse {
  def main(args: Array[String]) {

    val queryFile = InputFiles(args).Queries
    val queryParse = QueryParse(queryFile, includeSynonymsInQuery = true)

    //val queryParse = new QueryParse_old(queryFile)
    println(s"${queryParse.queries.size}")
    println(s"${queryParse.queries}")
  }
}