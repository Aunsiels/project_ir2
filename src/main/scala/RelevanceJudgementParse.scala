import scala.io.Source

case class RJTuple(docId: String, rel: String) 
 
class RelevanceJudgementParse(fname: String) {
  
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

object RelevanceJudgementParse {
  def main(args: Array[String]) {
    
    val dirname = "C:/Users/Michael/Desktop/IR Data/Project 2"
    val fname = dirname + "/relevance-judgements.csv" 
    var relelvanceParse = new RelevanceJudgementParse(fname)  
    
  }
}