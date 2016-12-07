import scala.collection.mutable.Map

class LanguageModel(idx : PersistentFreqIndex, 
                    path : String, 
                    options: TipsterOptions = TipsterOptions(),
                    useIndex: Boolean) extends ScoringModel {
  
  var documentLength = Map[String, Int]()
  var collectionFrequencies = Map[String, Int]()
  var nTermsInAllDocuments = 0.0
  var tfIndex = Map[String, List[FreqPosting]]()
  
  //use index if useIndex == true and index available
  if(useIndex && idx != null) {
    idx.index.foreach{
      index => 
        var cf = index._2.map(freqPosting => freqPosting.freq).sum
        index._2.foreach{
          freqPosting => 
          val docName = idx.getDocName(freqPosting.id)
          documentLength += docName -> (documentLength.getOrElse(docName, 0) + freqPosting.freq)
        }
    }
    tfIndex = idx.index
    collectionFrequencies = idx.index.map(termFPs => (termFPs._1, termFPs._2.map(fps => (fps.freq)).sum))
    nTermsInAllDocuments = documentLength.map(docFreq => docFreq._2).sum
  }
  
  override def getScores(queries: Map[String, List[String]], scoringOptions : ScoringModelOptions) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeScoreForQuery(query._2, scoringOptions)))
  }
  
  override def computeScoreForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)] = {
    var scoreMap = Map[String, Double]()
    
    //If we don't want to use index (use Index == false)
    //  or the index is not available
    // => need to make iterate through tipster stream first
    if(!(useIndex && idx != null)) {
      println("Analyzing query: " + query)
      documentLength = Map[String, Int]()
      collectionFrequencies = Map[String, Int]()
      nTermsInAllDocuments = 0.0
      tfIndex = Map[String, List[FreqPosting]]()
      val docs = new TipsterStreamSmart(path, options)
      val t = Timer(500, heapInfo = true)
      for (doc <- docs.stream) {
      /*docs.stream.foreach{
        doc =>*/ 
        t.progress(s"${doc.ID}, ${doc.name}") 
        for (tf <- doc.termFrequencies) {
          val queryTerm = tf._1
          if(query.contains(queryTerm)) {
            val freq = tf._2  
            tfIndex += queryTerm -> (tfIndex.getOrElse(queryTerm, List()) ++ List(FreqPosting(doc.ID, freq)))
          }
        }
        /*query.foreach{
          queryTerm => 
            var tf = doc.termFrequency(queryTerm)
            if(tf > 0) {
              tfIndex += queryTerm -> (tfIndex.getOrElse(queryTerm, List()) ++ List(FreqPosting(doc.ID, tf)))
            }
        }*/
        documentLength += doc.name -> doc.getDocumentLength
      }
      collectionFrequencies = tfIndex.map(termFPs => (termFPs._1, termFPs._2.map(fps => (fps.freq)).sum))
      nTermsInAllDocuments = documentLength.map(docFreq => docFreq._2).sum 
    }
    
    query.foreach{
      queryTerm => 
        val idxList = tfIndex.getOrElse(queryTerm, List())
        var cf = collectionFrequencies.getOrElse(queryTerm, 0)
        if(idxList.nonEmpty) {
          idxList.foreach{
            index =>
              val docName = idx.getDocName(index.id)
              var Pwd = math.log(1.0 + 
                    ((1.0 - scoringOptions.lambda) * (index.freq.toDouble / documentLength.getOrElse(docName, 0).toDouble))
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
     
    val options = TipsterOptions(maxDocs = 10000, chopping = -1)
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database
    val queryPath = infiles.Queries
    val relevancePath = infiles.Relevance
    val forceIndexRecreation = false
    
    val persistentIndex = new PersistentFreqIndex(docPath, dbPath, forceIndexRecreation, options)
   
    var includeSynonymsInQuery = false
    val queryParse = QueryParse(queryPath, options, includeSynonymsInQuery)
    val relevanceParse = new RelevanceJudgementParse_old(relevancePath)
    val relevance = RelevanceJudgementParse(relevancePath)
 
    val useIndex = false
    val languageModel = new LanguageModel(persistentIndex, docPath, options, useIndex)
    val sampleQuery = TipsterParseSmart.tokenize("aircraft dead whereabout adasdsdfasd", options)
    
    var lambda = 0.1
    var nDocsToBeReturned = 100
    val scoringOptions = new ScoringModelOptions(
        lambda = lambda, 
        nDocsToBeReturned = nDocsToBeReturned)
    languageModel.computeScoreForQuery(sampleQuery, scoringOptions)
    
    val scores = languageModel.getScores(queryParse.queries.slice(0, 4), scoringOptions)
    
    languageModel.convertScoresToListOfDocNames(scores).foreach{
      tfIdfScore =>
        println("Score for query: " + tfIdfScore._1)
        val stat = Evaluation.getStat(tfIdfScore._2, relevance.docs(tfIdfScore._1), 1)
        println(stat)
    } 
    
    val overallStats = Evaluation.getStat(languageModel.convertScoresToListOfDocNames(scores), relevance.docs, 1.0)
    println("Overall Stats for Language Model: ")
    println(overallStats)
        
  }
}