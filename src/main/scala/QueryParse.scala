/**
  * @author Michael
  *
  */

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.Map


/**
  * Read the query file and process all the queries
  *
  * @param fname
  * @param options
  */
case class QueryParse(fname: String,
                      options: TipsterOptions = TipsterOptions()) {

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


  val wordnet: Wordnet = if (options.useSynonyms) Wordnet() else null

  def insertQuery(num: String, title: String) = {
    val tit = if (options.useSynonyms) wordnet.expandBySynoyms(title) else title
    queries += num -> TipsterParseSmart.tokenize(tit, options)
  }

  Source.fromFile(fname).getLines()
    .foreach {
      case patNumber(num) => tempNum = num
      case patTitle(tit) =>
        tempTit = tit
        // very terrible hack, but need to do it.
        if (fname contains "test-questions.txt") {
          insertQuery(tempNum, tempTit)
          tempTit = ""
        }
      case patDesc(d) =>
        insertQuery(tempNum, tempTit)
        tempTit = ""
      case patCont(x) => if (!tempTit.isEmpty) tempTit ++= " " + x
      case _ =>
    }
  //println(queries.toSeq.sortWith(_._1 < _._1))

}

object QueryParse {
  def main(args: Array[String]) {

    val queryFile = InputFiles(args).Queries
    val queryParse = QueryParse(queryFile, options = TipsterOptions(useSynonyms = true))

    //val queryParse = new QueryParse_old(queryFile)
    println(s"${queryParse.queries.size}")
    println(s"${queryParse.queries}")
  }
}