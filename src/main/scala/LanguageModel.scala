import scala.collection.mutable.Map

class LanguageModel(idx : PersistentFreqIndex, 
                    path : String, 
                    options: TipsterOptions = TipsterOptions()) extends ScoringModel {
  
  var sumTFPerDoc = Map[String, Double]() //sum of the term frequencies of all terms in a document
  var collectionFrequencies = idx.index.map(termFPs => (termFPs._1, termFPs._2.map(fps => (fps.freq)).sum))
  idx.index.foreach{
     index => 
     var cf = index._2.map(freqPosting => freqPosting.freq).sum
     index._2.foreach{
       freqPosting => 
       val docName = idx.getDocName(freqPosting.id)
       sumTFPerDoc += docName -> (sumTFPerDoc.getOrElse(docName, 0.0) + freqPosting.freq)
     }
  }
  var nTermsInAllDocuments = sumTFPerDoc.map(docFreq => docFreq._2).sum
  
  override def getScores(queries: Map[String, List[String]], scoringOptions : ScoringModelOptions) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeScoreForQuery(query._2, scoringOptions)))
  }
  
  override def computeScoreForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)] = {
    var scoreMap = Map[String, Double]()
    query.foreach{
      queryTerm => 
        val idxList = idx.index.getOrElse(queryTerm, List())
        var cf = idxList.map(freqPosting => freqPosting.freq).sum
        if(idxList.nonEmpty) {
          idxList.foreach{
            index =>
              val docName = idx.getDocName(index.id)
              var Pwd = math.log(1.0 + 
                    ((1.0 - scoringOptions.lambda) * (index.freq.toDouble / sumTFPerDoc(docName).toDouble))
                    / (scoringOptions.lambda * (cf.toDouble / nTermsInAllDocuments.toDouble)))
              scoreMap += docName -> (scoreMap.getOrElse(docName, 0.0) + Pwd) 
          }
        }
    }
    val result = scoreMap.toSeq.sortWith(_._2 > _._2).take(100)
    result
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
 
    val languageModel = new LanguageModel(persistentIndex, docPath, options)
    val sampleQuery = TipsterParseSmart.tokenize("aircraft dead whereabout adasdsdfasd", options)
    
    var lambda = 0.1
    var nDocsToBeReturned = 100
    val scoringOptions = new ScoringModelOptions(lambda = lambda, nDocsToBeReturned = nDocsToBeReturned)
    languageModel.computeScoreForQuery(sampleQuery, scoringOptions)
    
    val scores = languageModel.getScores(queryParse.queries, scoringOptions)
    
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