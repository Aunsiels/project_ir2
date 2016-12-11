import scala.collection.mutable.Map

class LanguageModel(idx : PersistentFreqIndex, 
                    path : String, 
                    options: TipsterOptions = TipsterOptions(), override val useIndex: Boolean) extends ScoringModel {
   
  
  
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
    collectionFrequencies = idx.index.map(termFPs => (termFPs._1, termFPs._2.map(fps => fps.freq).sum))
    nTermsInAllDocuments = documentLength.values.sum
  }
  
  override def getScores(queries: Map[String, List[String]], scoringOptions : ScoringModelOptions) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeScoreForQuery(query._2, scoringOptions)))
  }
  
  override def computeScoreForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)] = {
    var scoreMap = Map[String, Double]()
    var result = Seq[(String, Double)]()
     
    //use index if useIndex == true and index available
    if(useIndex && idx != null) {
      query.foreach{
        queryTerm => 
          val idxList = tfIndex.getOrElse(queryTerm, List())
          val cf = collectionFrequencies.getOrElse(queryTerm, 0)
          if(idxList.nonEmpty) {
            idxList.foreach{
              index =>
                val docName = idx.getDocName(index.id)
                val Pwd = math.log(1.0 +
                  ((1.0 - scoringOptions.lambda) * (index.freq.toDouble / documentLength.getOrElse(docName, 0).toDouble))
                    / (scoringOptions.lambda * (cf.toDouble / nTermsInAllDocuments)))
                scoreMap += docName -> (scoreMap.getOrElse(docName, 0.0) + Pwd) 
            }
          }
        }
      result = scoreMap.toSeq.sortWith(_._2 > _._2).take(100)
    }
    //If we don't want to use index (use Index == false)
    //  or the index is not available
    // => need to iterate through tipster stream
    else {
      
      println("Analyzing query: " + query)
      
      //we start doing a first iteration over all documents
      //to get the collectionFrequencies and the total number
      //of terms in all the document
      //this information we need in the second round
      //for the Jelinek-Mercer smoothing
      val docs = new TipsterStreamSmart(path, options)
      collectionFrequencies = Map[String, Int]()
      nTermsInAllDocuments = 0.0
      var t = Timer(500, heapInfo = true)
      for (doc <- docs.stream) {
        t.progress(s"${doc.ID}, ${doc.name}") 
        for (tf <- doc.termFrequencies) {
          val term = tf._1
          val freq = tf._2  
          collectionFrequencies += term -> (collectionFrequencies.getOrElse(term, 0) + freq)
          nTermsInAllDocuments += freq
        }
      }
      
      //in the secound iteration over all documents
      //we compute the score for each document 
      //and keep a list of the top-n ranked documents
      t = Timer(500, heapInfo = true)
      for (doc <- docs.stream) {
        t.progress(s"${doc.ID}, ${doc.name}") 
        val documentLength = doc.getDocumentLength
        var docScore = 0.0
        for (tf <- doc.termFrequencies.filter(tf => query.contains(tf._1))) {
          val term = tf._1
          val freq = tf._2
          val cf = collectionFrequencies.getOrElse(term, 0)
          var Pwd = math.log(1.0 + 
                      ((1.0 - scoringOptions.lambda) * (freq.toDouble / documentLength.toDouble))
                      / (scoringOptions.lambda * (cf.toDouble / nTermsInAllDocuments)))
          docScore += Pwd  
        }
        if(docScore > 0) {
          result = result ++ Seq((doc.name, docScore)) 
          if(result.size > scoringOptions.nDocsToBeReturned) {
            result = result.sortWith(_._2 > _._2).take(scoringOptions.nDocsToBeReturned)
          }
        }
      }
    }
    result
  }   
}

object LanguageModel {
  def main(args: Array[String]): Unit = {
     
    val options = TipsterOptions(maxDocs = 10000, chopping = -1, useSynonyms = false)
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
 
    val useIndex = false
    val languageModel = new LanguageModel(persistentIndex, docPath, options, useIndex)
    val sampleQuery = TipsterParseSmart.tokenize("aircraft dead whereabout adasdsdfasd", options)

    val lambda = 0.1
    val nDocsToBeReturned = 100
    val scoringOptions = ScoringModelOptions(lambda = lambda, nDocsToBeReturned = nDocsToBeReturned)
    languageModel.computeScoreForQuery(sampleQuery, scoringOptions)
    
    val scores = languageModel.getScores(queryParse.queries, scoringOptions)
    
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