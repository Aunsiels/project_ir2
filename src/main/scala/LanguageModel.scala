import scala.collection.mutable.Map

class LanguageModel(idx : PersistentFreqIndex) {
  
  var sumTFPerDoc = Map[String, Double]() //sum of the term frequencies of all terms in a document
  var sumCFPerDoc = Map[String, Double]() //sum of the corpus frequencies of all terms in a document 
  idx.index.foreach{
     index => 
     var cf = index._2.map(freqPosting => freqPosting.freq).sum
     index._2.foreach{
       freqPosting => 
       sumTFPerDoc += freqPosting.name -> (sumTFPerDoc.getOrElse(freqPosting.name, 0.0) + freqPosting.freq)
       sumCFPerDoc += freqPosting.name -> (sumCFPerDoc.getOrElse(freqPosting.name, 0.0) + cf)
     }
  }
  
  def getScores(queries : Map[String, List[String]], lambda : Double) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeScoreForQuery(query._2, lambda)))
  }
  
  def computeScoreForQuery(query : List[String], lambda : Double) : Seq[(String, Double)] = {
    var scoreMap = Map[String, Double]()
    query.foreach{
      queryTerm => 
        val idxList = idx.index.getOrElse(queryTerm, List()) 
        var cf = idxList.map(freqPosting => freqPosting.freq).sum
        if(idxList.nonEmpty) {
          idxList.foreach{
            idx =>
              var Pwd = (1 - lambda) * math.log(idx.freq / sumTFPerDoc(idx.name)) + lambda * (cf / sumCFPerDoc(idx.name))
              scoreMap += idx.name -> (scoreMap.getOrElse(idx.name, 0.0) + Pwd) 
          }
          
        }
    }
    val result = scoreMap.toSeq.sortWith(_._2 < _._2).take(100)
    //println(result.size)
    result
  }
  
  def convertScoresToListOfDocNames(scores : Map[String, Seq[(String, Double)]]) : Map[Int, List[String]] =  {
    scores.map(scores => (scores._1.toInt, scores._2.map(tuple => tuple._1).toList))
  }
}

object LanguageModel {
  def main(args: Array[String]): Unit = {
     
    val options = TipsterOptions(maxDocs = 40000, chopping = -1)
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
 
    val languageModel = new LanguageModel(persistentIndex)
    val sampleQuery = TipsterParseSmart.tokenize("aircraft dead whereabout adasdsdfasd", options)
    
    var lambda = 0.1
    languageModel.computeScoreForQuery(sampleQuery, lambda)
    
    val scores = languageModel.getScores(queryParse.queries, lambda)
    
    languageModel.convertScoresToListOfDocNames(scores).foreach{
      tfIdfScore =>
        println("Score for query: " + tfIdfScore._1)
        val stat = Evaluation.getStat(tfIdfScore._2, relevance2.docs(tfIdfScore._1), 1)
        println(stat)
    } 
    
    val overallStats = Evaluation.getStat(languageModel.convertScoresToListOfDocNames(scores), relevance2.docs, 1.0)
    println("Overall Stats for Language Model: ")
    println(overallStats)
        
  }
}