/**
  * @author Michael
  */

import scala.collection.mutable
import scala.collection.mutable.Map

case class TfIdfVector(docId: String, rel: String)
case class TermFreqPosting(term: String, freq: Int) extends Ordered[TermFreqPosting] {
  def compare(that: TermFreqPosting) = this.term compare that.term
}

class TermBasedModel(idx : PersistentFreqIndex, 
                    path : String, 
                    options: TipsterOptions = TipsterOptions()) extends ScoringModel {
 
  var forwardIndex = Map[String, List[TermFreqPosting]]()
  
  var nDocs = idx.getAmountOfDocsInIndex()
  
  var docMaxFrequency = Map[String, Double]()
  idx.getDocNamesInIndex().foreach{
     docName => 
     docMaxFrequency += docName -> 0.0           
  }
  idx.index.foreach{
     index => 
     index._2.foreach{
       freqPosting => 
       val docName = idx.getDocName(freqPosting.id)
       if(freqPosting.freq > docMaxFrequency.getOrElse(docName, 0.0)) {
         docMaxFrequency(docName) = freqPosting.freq
       }
       //forwardIndex(docName) = forwardIndex.getOrElse(docName, List()) ++ List(TermFreqPosting(index._1, freqPosting.freq))
     }
  }  
  
  println("forwardIndex created")
   
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
       val docName = idx.getDocName(freqPosting.id)
       docVectorNorms(docName) += (tfidf * tfidf)
     }
  }
  
  docVectorNorms = docVectorNorms.map(norms => (norms._1, math.sqrt(norms._2)))
  
  override def getScores(queries: Map[String, List[String]], scoringOptions : ScoringModelOptions) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeScoreForQuery(query._2, scoringOptions)))
  }
  
  override def computeScoreForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)] = {
    if(scoringOptions.scoringMethod == "TFIDF") {
      computeTfIdfScoresForQuery(query, scoringOptions.nDocsToBeReturned)
    } else if(scoringOptions.scoringMethod == "COSINEDISTANCE") {
      this.computeCosineDistancesForQuery(query, scoringOptions.nDocsToBeReturned) 
    }
    else {
      Seq[(String, Double)]()
    }
  }
  
  def computeCosineDistancesForQuery(query : List[String], nDocsToReturn : Int) : Seq[(String, Double)] = {
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
          index => 
          val tfdoc = index.freq
          val tfidfdoc = math.log(1 + tfdoc) * idf
          val tfidfquery = math.log(1 + queryTerm._2) * idf
          val docName = idx.getDocName(index.id)
          cosineDistanceMap += docName -> (cosineDistanceMap.getOrElse(docName, 0.0) + (tfidfdoc * tfidfquery)) 
        }
      }      
    }
    queryVectorNorm = math.sqrt(queryVectorNorm)
    //println(queryVectorNorm)
    //println(cosineDistanceMap)
    cosineDistanceMap = cosineDistanceMap.map(idDist => (idDist._1, idDist._2 / (queryVectorNorm * docVectorNorms(idDist._1))))
    //println(cosineDistanceMap)
    val result = cosineDistanceMap.toSeq.sortWith(_._2 > _._2).take(nDocsToReturn)
    //val result = cosineDistanceMap.toSeq.sortWith(_._2 > _._2)
    //println(result)
    result
  }
  
  def computeTfIdfScoresForQuery(query : List[String], nDocsToReturn : Int) : Seq[(String, Double)] = {
    //println("computing score for query terms: " + query)
    var scoreMap = Map[String, Double]()
    query.foreach{
      queryTerm => 
      val idxList = idx.index.getOrElse(queryTerm, List()) 
      if(idxList.nonEmpty) {
        val df = idxList.size
        val idf = math.log(nDocs / df)
        idxList.foreach{
          index => 
          val docName = idx.getDocName(index.id)
          val tf = index.freq
          //val tfNorm = (tf / docMaxFrequency(docName))
          val augmentedTf = (0.5 + (0.5 * (tf / docMaxFrequency(docName)))) 
          //val tfidf = math.log(1 + tfNorm) * idf
          val tfidf = augmentedTf * idf
          scoreMap += docName -> (scoreMap.getOrElse(docName, 0.0) + tfidf) 
        }
      }      
    }
    val result = scoreMap.toSeq.sortWith(_._2 > _._2).take(nDocsToReturn)
    //val result = scoreMap.toSeq.sortWith(_._2 > _._2)
    //println(result)
    result
  }
  
  def queryExpansion(origQueries : Map[String, List[String]], relevantDocs : Map[Int, List[String]], nDocsToAdd : Int) :  Map[String, List[String]] = {
    origQueries.map(
      origQuery => (origQuery._1 -> (
        relevantDocs(origQuery._1.toInt).map(
          relevantDocForQuery =>
            origQuery._2 ++ forwardIndex(relevantDocForQuery).sortBy(termFP => termFP.freq).take(nDocsToAdd).map(termFP => termFP.term)
        )
       ).flatten)
    )
  }
}

object TermBasedModel {
  def main(args: Array[String]): Unit = {
     
    val options = TipsterOptions(maxDocs = 100000, chopping = -1, ngramSize = 0)
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database
    val queryPath = infiles.Queries
    val relevancePath = infiles.Relevance
    val forceIndexRecreation = false

    val persistentIndex = new PersistentFreqIndex(docPath, dbPath, forceIndexRecreation, options)
   
    val queryParse = QueryParse(queryPath, options)
    val relevanceParse = new RelevanceJudgementParse_old(relevancePath)
    val relevance = RelevanceJudgementParse(relevancePath)
 
    val termModel = new TermBasedModel(persistentIndex, docPath, options)
     
    val nDocsToBeReturned = 100
    val scoringMethod = "TFIDF"
    val scoringOptions = new ScoringModelOptions(
        nDocsToBeReturned = nDocsToBeReturned,
        scoringMethod = scoringMethod)
    
    /*val sampleQuery = TipsterParseSmart.tokenize("along aircraft dead whereabout adasdsdfasd", options)
    println(sampleQuery) 
    var result = termModel.computeScoreForQuery(sampleQuery, scoringOptions)
    println(result)
    println(result.size)*/
    
    var scores = termModel.getScores(queryParse.queries, scoringOptions)
    termModel.convertScoresToListOfDocNames(scores).foreach{
      tfIdfScore =>
        println("Score for query: " + tfIdfScore._1)
        println("relevant docs: " + relevance.docs(tfIdfScore._1))
        println("proposed docs: " + tfIdfScore._2)
        val stat = Evaluation.getStat(tfIdfScore._2, relevance.docs(tfIdfScore._1), 1)
        println(stat)
    }
    val overallStat = Evaluation.getStat(termModel.convertScoresToListOfDocNames(scores), relevance.docs, 1.0)
    println("Overall Stats: ")
    println(overallStat)
    
     
    /*var tfIdfScores = termModel.getScores(queryParse.queries, scoringOptions)
    var nTermsToIncludeInQuery = 10
    //var expandedQueries = termModel.queryExpansion(queryParse.queries, termModel.convertScoresToListOfDocNames(tfIdfScores), nTermsToIncludeInQuery)
    println("queries expanded")
    //tfIdfScores = termModel.getTfIdfScores(expandedQueries, nDocsToReturn)
    //println(expandedQueries)*/
  }
}