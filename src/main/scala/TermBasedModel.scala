import ch.ethz.dal.tinyir.io._

class TermBasedModel(idx : PersistentFreqIndex)  {
    
  def computeScoreForSingleQuery(query : String, nDocs : Int) {
    
    val setOfQueryTerms = query.split(" ").toSet
    println("computing score for query terms: " + setOfQueryTerms)
    var scoreMap = Map[Int, Double]()
    setOfQueryTerms.foreach{
      queryTerm =>
      val idxList = idx.index.getOrElse(queryTerm, List()) 
      if(idxList.size > 0) {
        val df = idxList.size
        val idf = math.log((nDocs / df))
        idxList.foreach{
          idx =>
          val tf = idx.freq
          val tfidf = math.log(1 + tf) * idf
          scoreMap += idx.id -> (scoreMap.getOrElse(idx.id, 0.0) + tfidf) 
        }
        
      }      
    }
    println(scoreMap.toSeq.sortWith(_._2 > _._2).take(100))
  }
  
}

object TermBasedModel {
  def main(args: Array[String]) = {
        
    val nDocs = 100
    val docPath = "C:/Users/Michael/Desktop/IR Data/Project 2/documents/"
    val dbPath = "C:/Users/Michael/Desktop/indexDb2"
    //val tipsterStream = new TipsterStream(docPath).stream.dropRight(100000 - nDocs)   
    val recomputeIndex = false
    //var idx = new PersistentFreqIndex(null, dbPath, recomputeIndex)
    var idx = new PersistentFreqIndex(docPath, nDocs, dbPath, recomputeIndex)  
    val dirname = "C:/Users/Michael/Desktop/IR Data/Project 2"
    val fname = dirname + "/questions-descriptions.txt"
    val queryParse = new QueryParse(fname)
    val termModel = new TermBasedModel(idx)
    //var sampleQuery = queryParse.queries("053")
    var sampleQuery = "quakepredict unionactivist whereabout adasdsdfasd"
    termModel.computeScoreForSingleQuery(sampleQuery, 10000)
    
    println("finished")
        
  }
}