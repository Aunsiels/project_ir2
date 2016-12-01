import ch.ethz.dal.tinyir.io._
import scala.collection.mutable.Map

case class TfIdfVector(docId: String, rel: String) 

class TermBasedModel(idx : PersistentFreqIndex)  {
 
  var nDocs = idx.getAmountOfDocsInIndex()
  println("Amount of Documents in Index: " + nDocs) 
   
  var docVectorNorms = Map[String, Double]()
  idx.getDocNamesInIndex().foreach{
     docId => 
     docVectorNorms += docId -> 0            
  }
  idx.index.foreach{
     index => 
     val df = index._2.size
     val idf = math.log(nDocs / df)
     index._2.foreach{
       freqPosting => 
       var tf = freqPosting.freq
       var tfidf = math.log(1 + tf) * idf
       docVectorNorms(freqPosting.name) += (tfidf * tfidf)
     }
  }
  
  docVectorNorms = docVectorNorms.map(norms => (norms._1, math.sqrt(norms._2)))
  
  def getCosineDistances(queries : Map[String, List[String]]) : Map[String, Seq[(String, Double)]] = {
    queries.map(query => (query._1, computeCosineDistancesForQuery(query._2)))
  }
  
  def computeCosineDistancesForQuery(query : List[String]) : Seq[(String, Double)] = {
    var queryTermFrequency = query.groupBy(identity).mapValues(t => t.length)
    println("computing score for query terms: " + queryTermFrequency)
    var cosineDistanceMap = Map[String, Double]()
    
    var queryVectorNorm = 0.0
    queryTermFrequency.foreach{
      queryTerm => 
      queryVectorNorm += (queryTerm._2 * queryTerm._2)
      val idxList = idx.index.getOrElse(queryTerm._1, List()) 
      if(idxList.size > 0) {        
        val df = idxList.size
        val idf = math.log(nDocs / df)
        idxList.foreach{
          idx => 
          var tfdoc = idx.freq
          var tfidfdoc = math.log(1 + tfdoc) * idf
          var tfidfquery = math.log(1 + queryTerm._2) * idf          
          cosineDistanceMap += idx.name -> (cosineDistanceMap.getOrElse(idx.name, 0.0) + (tfidfdoc * tfidfquery)) 
        }
      }      
    }
    queryVectorNorm = math.sqrt(queryVectorNorm)
    //println(queryVectorNorm)
    //println(cosineDistanceMap)
    cosineDistanceMap = cosineDistanceMap.map(idDist => (idDist._1, (idDist._2 / (queryVectorNorm * docVectorNorms(idDist._1))))) 
    //println(cosineDistanceMap)
    val result = cosineDistanceMap.toSeq.sortWith(_._2 > _._2).take(100)
    println(result)
    result
  }
  
  def getTfIdfScores(queries : Map[String, List[String]]) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeTfIdfScoresForQuery(query._2)))
  }
  
  def computeTfIdfScoresForQuery(query : List[String]) : Seq[(String, Double)] = {
    println("computing score for query terms: " + query)
    var scoreMap = Map[String, Double]()
    query.foreach{
      queryTerm => 
      val idxList = idx.index.getOrElse(queryTerm, List()) 
      if(idxList.nonEmpty) {
        val df = idxList.size
        val idf = math.log(nDocs / df)
        idxList.foreach{
          idx => 
          var tf = idx.freq
          var tfidf = math.log(1 + tf) * idf
          scoreMap += idx.name -> (scoreMap.getOrElse(idx.name, 0.0) + tfidf) 
        }
      }      
    }
    var result = scoreMap.toSeq.sortWith(_._2 > _._2).take(100)
    println(result)
    result
  }
  
  def convertScoresToSetOfDocNames(scores : Map[String, Seq[(String, Double)]]) : Map[String, Set[String]] =  {
    scores.map(scores => (scores._1, scores._2.map(tuple => tuple._1).toSet))
  }
  
}

object TermBasedModel {
  def main(args: Array[String]): Unit = {
     
    val nDocs = 10000
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database
    val queryPath = infiles.Queries
    val relevancePath = infiles.Relevance
    val forceIndexRecreation = false

    val persistentIndex = new PersistentFreqIndex(docPath, dbPath, nDocs, forceIndexRecreation)
   
    val queryParse = new QueryParse(queryPath)   
    var relelvanceParse = new RelevanceJudgementParse(relevancePath)  
 
    val termModel = new TermBasedModel(persistentIndex)
    var sampleQuery = List("aircra",  "dead",  "whereabout",  "adasdsdfasd")
    
    termModel.computeTfIdfScoresForQuery(sampleQuery)
    
    var tfIdfScores = termModel.getTfIdfScores(queryParse.queries)
    
    termModel.computeCosineDistancesForQuery(sampleQuery)
    
    var cosineDistances = termModel.getCosineDistances(queryParse.queries)
    
    termModel.convertScoresToSetOfDocNames(cosineDistances).foreach{
      cosineDistance =>
        println(cosineDistance)
        var stat = Evaluation.getStat(cosineDistance._2, relelvanceParse.getRelevantDocsForQuery(cosineDistance._1.toInt), 1)
        println(stat)
    }
    
    return       
  }
}