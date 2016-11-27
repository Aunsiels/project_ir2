import ch.ethz.dal.tinyir.io._
import scala.collection.mutable.Map

case class TfIdfVector(docId: String, rel: String) 

class TermBasedModel(idx : PersistentFreqIndex)  {
 
  var nDocs = idx.getAmountOfDocsInIndex()
  println("Amount of Documents in Index: " + nDocs)
  println("min index: "+ idx.getDocsInIndex().min)
  println("max index: "+ idx.getDocsInIndex().max)  
  
  /*var vectors = Map[Int, DenseVector[Double]]()
  idx.getDocsInIndex().foreach{
     docId => docId
     vectors += docId -> DenseVector.zeros[Double](idx.index.size)            
  };*/  
  
  var docVectorNorms = Map[Int, Double]()
  idx.getDocsInIndex().foreach{
     docId => 
     docVectorNorms += docId -> 0            
  };
  idx.index.foreach{
     index => 
     val df = index._2.size
     val idf = math.log((nDocs / df))
     index._2.foreach{
       freqPosting => 
       var tf = freqPosting.freq
       var tfidf = math.log(1 + tf) * idf
       docVectorNorms(freqPosting.id) += (tfidf * tfidf)
     }
  }
  
  docVectorNorms = docVectorNorms.map(norms => (norms._1, math.sqrt(norms._2)))
  
  def getCosineDistances(queries : Map[String, String]) : Map[String, Seq[(Int, Double)]] = {
    queries.map(query => (query._1, computeTfIdfScoresForQuery(query._2)))
  }
  
  def computeCosineDistancesForQuery(query : String) /*: Seq[(Int, Double)]*/ = {
    var queryTermFrequency = query.split(" ").groupBy(identity).mapValues(t => t.length)
    println("computing score for query terms: " + queryTermFrequency)
    var cosineDistanceMap = Map[Int, Double]()
    
    var queryVectorNorm = 0.0
    queryTermFrequency.foreach{
      queryTerm => 
      queryVectorNorm += (queryTerm._2 * queryTerm._2)
      val idxList = idx.index.getOrElse(queryTerm._1, List()) 
      if(idxList.size > 0) {        
        val df = idxList.size
        val idf = math.log((nDocs / df))
        idxList.foreach{
          idx => 
          var tfdoc = idx.freq
          var tfidfdoc = math.log(1 + tfdoc) * idf
          var tfidfquery = math.log(1 + queryTerm._2) * idf          
          cosineDistanceMap += idx.id -> (cosineDistanceMap.getOrElse(idx.id, 0.0) + (tfidfdoc * tfidfquery)) 
        }
      }      
    }
    queryVectorNorm = math.sqrt(queryVectorNorm)
    //println(queryVectorNorm)
    //println(cosineDistanceMap)
    cosineDistanceMap = cosineDistanceMap.map(idDist => (idDist._1, (idDist._2 / (queryVectorNorm * docVectorNorms(idDist._1))))) 
    //println(cosineDistanceMap)
    var result = cosineDistanceMap.toSeq.sortWith(_._2 > _._2).take(100).toSeq
    println(result)
    result
  }
  
  def getTfIdfScores(queries : Map[String, String]) : Map[String, Seq[(Int, Double)]] =  {
    queries.map(query => (query._1, computeTfIdfScoresForQuery(query._2)))
  }
  
  def computeTfIdfScoresForQuery(query : String) : Seq[(Int, Double)] = {
    var listOfQueryTerms = query.split(" ").toList
    println("computing score for query terms: " + listOfQueryTerms)
    var scoreMap = Map[Int, Double]()
    listOfQueryTerms.foreach{
      queryTerm => 
      val idxList = idx.index.getOrElse(queryTerm, List()) 
      if(idxList.size > 0) {
        val df = idxList.size
        val idf = math.log((nDocs / df))
        idxList.foreach{
          idx => 
          var tf = idx.freq
          var tfidf = math.log(1 + tf) * idf
          scoreMap += idx.id -> (scoreMap.getOrElse(idx.id, 0.0) + tfidf) 
        }
      }      
    }
    var result = scoreMap.toSeq.sortWith(_._2 > _._2).take(100).toSeq
    println(result)
    result
  }
  
}

object TermBasedModel {
  def main(args: Array[String]) = {
        
    val nDocs = 1000
    val docPath = "C:/Users/Michael/Desktop/IR Data/Project 2/documents/"
    val dbPath = "C:/Users/Michael/Desktop/indexDatabases/dbWithIndexOf1000Docs"
    //val tipsterStream = new TipsterStream(docPath).stream.dropRight(100000 - nDocs)   
    val recomputeIndex = false
    //var idx = new PersistentFreqIndex(null, dbPath, recomputeIndex)
    var idx = new PersistentFreqIndex(docPath, nDocs, dbPath, recomputeIndex)  
    val dirname = "C:/Users/Michael/Desktop/IR Data/Project 2"
    val fname = dirname + "/questions-descriptions.txt"
    val queryParse = new QueryParse(fname)    
    val fname2 = dirname + "/relevance-judgements.csv" 
    var relelvanceParse = new RelevanceJudgementParse(fname2)  
 
    val termModel = new TermBasedModel(idx)
    var sampleQuery = "quakepredict unionactivist whereabout adasdsdfasd quakepredict"
    
    termModel.computeTfIdfScoresForQuery(sampleQuery)
    
    var scores1 = termModel.getTfIdfScores(queryParse.queries)
    var scores1WithDocNames = scores1.mapValues(scores => scores.map(docIdScore => (idx.namesMap(docIdScore._1), docIdScore._2)))
    println(scores1WithDocNames)
    
    termModel.computeCosineDistancesForQuery(sampleQuery)
    
    var scores2 = termModel.getCosineDistances(queryParse.queries)
    var scores2WithDocNames = scores2.mapValues(scores => scores.map(docIdScore => (idx.namesMap(docIdScore._1), docIdScore._2)))
    println(scores2WithDocNames)
    
    var docNames2 = scores2WithDocNames.mapValues(scores => scores.map(scores => scores._1).toSet)
    println(docNames2)
     
    docNames2.foreach{
      queryIdDocs =>
        //println(relelvanceParse.getRelevantDocsForQuery(queryIdDocs._1.toInt))
        //println(queryIdDocs._2)
        var stat = Evaluation.getStat(queryIdDocs._2, relelvanceParse.getRelevantDocsForQuery(queryIdDocs._1.toInt), 1)
        println(stat)
    }
    
    println("finished")
        
  }
}