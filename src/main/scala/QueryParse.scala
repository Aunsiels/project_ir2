/**
  * @author Michael
  *
  */

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.Map


case class QueryParse(fname: String, options: TipsterOptions = TipsterOptions()) {

  var queries = collection.mutable.Map[String, List[String]]()

  /* Parse entries that look like this. Note that title can extend lines until the next <desc>
  <num> Number: 090

  <dom> Domain: International Economics

  <title> Topic: Data on Proven Reserves of Oil & Natural Gas
  Producers
  */

  val patNumber = "<num>\\s+Number:\\s+(\\d+)\\s*".r
  val patTitle = "<title>\\s+Topic:\\s+(.+)\\s*".r
  val patCont = "\\s*(\\S.*)\\s*".r
  val patDesc = "<desc>(.+)".r

  var tempNum = ""
  var tempTit = ""

  Source.fromFile(fname).getLines()
    .foreach {
      case patNumber(num) => tempNum = num
      case patTitle(tit) => tempTit = tit
      case patDesc(d) =>
        queries += tempNum -> TipsterParseSmart.tokenize(tempTit, options)
        tempTit = ""
      case patCont(x) => if (!tempTit.isEmpty) tempTit ++= " " + x
      case _ =>
    }
  //println(queries.toSeq.sortWith(_._1 < _._1))

}

class QueryParse_old(fname: String) {
  
  private var topics = mutable.Map[String, List[String]]()
  
  def queries : Map[String, List[String]] = this.topics 
     
  private var number : String = ""
  private var topic : String = ""    
  for(line <- Source.fromFile(fname).getLines()) {
     if(line.contains("<num>")) {
        number = line.substring(line.indexOf(':')+1).replace(" ", "")
      }
      if(line.contains("<title>")) {
        topic = line.substring(line.indexOf(':') + 1)
        while(topic.startsWith(" ")) {
          topic = topic.substring(1)
        }
        val query = TipsterParseSmart.tokenize(topic, numbers = true, stops = true, stemming = true)
        topics += number -> query
      }
    }
  //println(topics)
  //println(topics.size)

}

object QueryParse {
  def main(args: Array[String]) {

    val queryFile = InputFiles(args).Queries
    val queryParse = QueryParse(queryFile)

    //val queryParse = new QueryParse_old(queryFile)
    println(s"${queryParse.queries.size}")
    println(s"${queryParse.queries}")
  }
}