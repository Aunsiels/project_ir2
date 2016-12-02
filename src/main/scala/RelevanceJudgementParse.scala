/**
  * @author Michael
  * @author mmgreiner
  */

import scala.io.Source

case class RJTuple(docId: String, rel: String) 
 
class RelevanceJudgementParse_old(fname: String) {
  
  var relevanceMap = Map[Int, List[RJTuple]]()
  var lastQueryId = -1
  var lastQueryRelList = List[RJTuple]()
  for(line <- Source.fromFile(fname).getLines()) {
    val lineList = line.split(" ") 
    val queryId = lineList(0).toInt
    if(queryId != lastQueryId) {
      relevanceMap += queryId -> lastQueryRelList
      lastQueryRelList = List[RJTuple]()
      lastQueryId = queryId
    }
    lastQueryRelList ::= RJTuple(lineList(2).replace("-", ""), lineList(3))
  }  
  //println(RJMap)
  //println(RJMap.size)
  
  def getRelevantDocsForQuery(queryId : Int) : Set[String] = {
    relevanceMap.getOrElse(queryId, List()).filter(rjtuple => rjtuple.rel == "1").map(rjtuple => rjtuple.docId).toSet
  }
}

/**
  * The entries in the relevance file are like this:
  * 89 0 AP880920-0242 1
  * The first number is the query id
  * the third column is the document number. it has to be cleared, ie all - removed.
  * The last 1 indicates, that this document is relevant
  * Detect all the relevant documents for all queries.
  *
  * @example val reldocs = RelevantDocs("relevance-judgements.csv")
  *
  * @param fname
  */
case class RelevanceJudgementParse(fname: String) {
  private val queries = collection.mutable.Map[Int, List[String]]()
  val pat = """(\d+)\s+(\d)\s+(.+)\s+1s*""".r   // 89 0 AP880920-0242 0    only those with a 1 are relevant
  Source.fromFile(fname).getLines.foreach {
    case pat(id, ignore, doc) =>
      queries += id.toInt -> (doc.replace("-", "") :: queries.getOrElse(id.toInt, List[String]()))
    case _ =>
  }
  val queryAndDocs = queries.toMap[Int, List[String]]

  /**
    * two simple access functions
    */
  def docs(queryId: Int): Set[String] = queries(queryId).toSet
  def docs(queryId: String): Set[String] = queries(queryId.toInt).toSet
}

object RelevanceJudgementParse {
  def main(args: Array[String]) {

    val inf = InputFiles(args)
    val relevanceParse = RelevanceJudgementParse(inf.Relevance)
    println(s"success with ${inf.Relevance}, size relevance queries: ${relevanceParse.queryAndDocs.size}")
  }
}