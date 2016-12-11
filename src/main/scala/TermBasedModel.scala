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
                     options: TipsterOptions = TipsterOptions(),
                     override val useIndex: Boolean) extends ScoringModel {
 
  var nDocs = 0
  val docMaxFrequency = Map[String, Double]()
  var docVectorNorms = Map[String, Double]()
  nDocs = idx.getAmountOfDocsInIndex()
  
  idx.index.foreach{
     index => 
     val df = index._2.size
     val idf = math.log(nDocs / df)
     index._2.foreach{
       freqPosting => 
       val tf = freqPosting.freq
       val tfidf = math.log(1 + tf) * idf
       val docName = idx.getDocName(freqPosting.id)
       docVectorNorms += docName -> (docVectorNorms.getOrElse(docName,0.0) + (tfidf * tfidf))
       if(freqPosting.freq > docMaxFrequency.getOrElse(docName, 0.0)) {
         docMaxFrequency(docName) = freqPosting.freq
       }
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
    
    if(idx.index.isEmpty) {
      //TODO: need to run the scoring directly on the stream
    }
    
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
    val forwardIndex = Map[String, List[TermFreqPosting]]()
    idx.index.foreach{
      index => 
       index._2.foreach{
         freqPosting => 
         val docName = idx.getDocName(freqPosting.id)
         forwardIndex(docName) = forwardIndex.getOrElse(docName, List()) ++ List(TermFreqPosting(index._1, freqPosting.freq))
       }
    } 
    println("forwardIndex created")
    origQueries.map(
      origQuery => (origQuery._1 -> relevantDocs(origQuery._1.toInt).flatMap(relevantDocForQuery =>
        origQuery._2 ++ forwardIndex(relevantDocForQuery).sortBy(termFP => termFP.freq).take(nDocsToAdd).map(termFP => termFP.term)))
    )
  }
}

object TermBasedModel {

  def run(infiles: InputFiles,  options: TipsterOptions, queryParse: QueryParse, index: PersistentFreqIndex = null) = {

    val relevance = RelevanceJudgementParse(infiles.Relevance)

    val termModel = new TermBasedModel(index, infiles.DocPath, options, index != null)

    val scoringOptions = ScoringModelOptions(scoringMethod = "COSINEDISTANCE")

    val scores = termModel.getScores(queryParse.queries, scoringOptions)
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
  }

  def main(args: Array[String]): Unit = {
     
    val options = TipsterOptions(maxDocs = 10000, chopping = -1, useSynonyms = false)
    val infiles = InputFiles(args)
    val writeIndex = true

    val persistentIndex = new PersistentFreqIndex(infiles.DocPath, infiles.Database, writeIndex, options)

    val queryParse = QueryParse(infiles.Queries, options)
    run(infiles, options, queryParse)

  }
}