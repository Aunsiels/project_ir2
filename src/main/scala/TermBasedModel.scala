/**
  * @author Michael
  */

import scala.collection.mutable
import scala.collection.mutable.Map

case class TfIdfVector(docId: String, rel: String) 

class TermBasedModel(idx : PersistentFreqIndex)  {
 
  var nDocs = idx.getAmountOfDocsInIndex()
  //println("Amount of Documents in Index: " + nDocs) 
   
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
       val tf = freqPosting.freq
       val tfidf = math.log(1 + tf) * idf
       docVectorNorms(freqPosting.name) += (tfidf * tfidf)
     }
  }
  
  docVectorNorms = docVectorNorms.map(norms => (norms._1, math.sqrt(norms._2)))
  
  def getCosineDistances(queries : Map[String, List[String]]) : Map[String, Seq[(String, Double)]] = {
    queries.map(query => (query._1, computeCosineDistancesForQuery(query._2)))
  }
  
  def computeCosineDistancesForQuery(query : List[String]) : Seq[(String, Double)] = {
    val queryTermFrequency = query.groupBy(identity).mapValues(t => t.length)
    //println("computing score for query terms: " + queryTermFrequency)
    var cosineDistanceMap = Map[String, Double]()
    
    var queryVectorNorm = 0.0
    queryTermFrequency.foreach{
      queryTerm => 
      queryVectorNorm += (queryTerm._2 * queryTerm._2)
      val idxList = idx.index.getOrElse(queryTerm._1, List()) 
      if(idxList.nonEmpty) {
        val df = idxList.size
        val idf = math.log(nDocs / df)
        idxList.foreach{
          idx => 
          val tfdoc = idx.freq
          val tfidfdoc = math.log(1 + tfdoc) * idf
          val tfidfquery = math.log(1 + queryTerm._2) * idf
          cosineDistanceMap += idx.name -> (cosineDistanceMap.getOrElse(idx.name, 0.0) + (tfidfdoc * tfidfquery)) 
        }
      }      
    }
    queryVectorNorm = math.sqrt(queryVectorNorm)
    //println(queryVectorNorm)
    //println(cosineDistanceMap)
    cosineDistanceMap = cosineDistanceMap.map(idDist => (idDist._1, idDist._2 / (queryVectorNorm * docVectorNorms(idDist._1))))
    //println(cosineDistanceMap)
    val result = cosineDistanceMap.toSeq.sortWith(_._2 > _._2).take(100)
    //val result = cosineDistanceMap.toSeq.sortWith(_._2 > _._2)
    //println(result)
    result
  }
  
  def getTfIdfScores(queries : Map[String, List[String]]) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeTfIdfScoresForQuery(query._2)))
  }
  
  def computeTfIdfScoresForQuery(query : List[String]) : Seq[(String, Double)] = {
    //println("computing score for query terms: " + query)
    var scoreMap = Map[String, Double]()
    query.foreach{
      queryTerm => 
      val idxList = idx.index.getOrElse(queryTerm, List()) 
      if(idxList.nonEmpty) {
        val df = idxList.size
        val idf = math.log(nDocs / df)
        idxList.foreach{
          idx => 
          val tf = idx.freq
          val tfidf = math.log(1 + tf) * idf
          scoreMap += idx.name -> (scoreMap.getOrElse(idx.name, 0.0) + tfidf) 
        }
      }      
    }
    val result = scoreMap.toSeq.sortWith(_._2 > _._2).take(100)
    //val result = scoreMap.toSeq.sortWith(_._2 > _._2)
    //println(result)
    result
  }
  
  def convertScoresToListOfDocNames(scores : Map[String, Seq[(String, Double)]]) : Map[Int, List[String]] =  {
    scores.map(scores => (scores._1.toInt, scores._2.map(tuple => tuple._1).toList))
  }
  
}

object TermBasedModel {
  def main(args: Array[String]): Unit = {
     
    val options = TipsterOptions(maxDocs = 100000, chopping = -1)
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database
    val queryPath = infiles.Queries
    val relevancePath = infiles.Relevance
    val forceIndexRecreation = false

    val persistentIndex = new PersistentFreqIndex(docPath, dbPath, forceIndexRecreation, options)
   
    val queryParse = QueryParse(queryPath, options)
    val relevanceParse = new RelevanceJudgementParse_old(relevancePath)
    val relevance2 = RelevanceJudgementParse(relevancePath)
 
    val termModel = new TermBasedModel(persistentIndex)
    val sampleQuery = TipsterParseSmart.tokenize("aircraft dead whereabout adasdsdfasd", options)
    
    termModel.computeTfIdfScoresForQuery(sampleQuery)
    
    val tfIdfScores = termModel.getTfIdfScores(queryParse.queries)
    
    termModel.convertScoresToListOfDocNames(tfIdfScores).foreach{
      tfIdfScore =>
        println("Score for query: " + tfIdfScore._1)
        val stat = Evaluation.getStat(tfIdfScore._2, relevance2.docs(tfIdfScore._1), 1)
        println(stat)
    }   
    
    termModel.computeCosineDistancesForQuery(sampleQuery)
    
    val cosineDistances = termModel.getCosineDistances(queryParse.queries)
    
    termModel.convertScoresToListOfDocNames(cosineDistances).foreach{
      cosineDistance =>
        println("Score for query: " + cosineDistance._1)
        val stat = Evaluation.getStat(cosineDistance._2, relevance2.docs(cosineDistance._1), 1)
        println(stat)
    }
    
    val overallStatsCosineDistances = Evaluation.getStat(termModel.convertScoresToListOfDocNames(cosineDistances), relevance2.docs, 1.0)
    println("Overall Stats CosineDistances: ")
    println(overallStatsCosineDistances)
        
    val overallStatTfIdfScores = Evaluation.getStat(termModel.convertScoresToListOfDocNames(tfIdfScores), relevance2.docs, 1.0)
    println("Overall Stats TfIdfScores: ")
    println(overallStatTfIdfScores)
  }
}