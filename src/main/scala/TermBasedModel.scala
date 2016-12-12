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
  
  //use index if useIndex == true and index available
  if(useIndex && idx != null) {
    nDocs = idx.getAmountOfDocsInIndex()
    idx.index.foreach{
       index => 
       val df = index._2.size
       val idf = math.log(nDocs.toDouble / df.toDouble)
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
  }
  
  override def getScores(queries: Map[String, List[String]], scoringOptions : ScoringModelOptions) : Map[String, Seq[(String, Double)]] =  {
    queries.map(query => (query._1, computeScoreForQuery(query._2, scoringOptions)))
  }
  
  override def computeScoreForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)] = {
    if(scoringOptions.scoringMethod == "TFIDF") {
      computeTfIdfScoresForQuery(query, scoringOptions)
    } else if(scoringOptions.scoringMethod == "COSINESIMILARITY") {
      this.computeCosineSimilarityForQuery(query, scoringOptions) 
    }
    else {
      Seq[(String, Double)]()
    }
  }
  
  def computeCosineSimilarityForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)] = {
    val queryTermFrequency = collection.mutable.Map[String, Int]()
    query.foreach(t => {
      queryTermFrequency += t -> (1 + queryTermFrequency.getOrElse(t, 0))
    })
    var scoreMap = Map[String, Double]()
    var result = Seq[(String, Double)]()
    
    //use index if useIndex == true and index available
    if(useIndex && idx != null) {
      var queryVectorNorm = 0.0
      queryTermFrequency.foreach{
        queryTerm => 
        val idxList = idx.index.getOrElse(queryTerm._1, List()) 
        val df = idxList.size
          val idf = if (df > 0) math.log(nDocs.toDouble / df.toDouble) else 0
          val tfidfquery = math.log(1 + queryTerm._2) * idf
          queryVectorNorm += (tfidfquery * tfidfquery)
          
        if(idxList.nonEmpty) {
          idxList.foreach{
            index => 
            val tfdoc = index.freq
            val tfidfdoc = math.log(1 + tfdoc) * idf
            val docName = idx.getDocName(index.id)
            scoreMap += docName -> (scoreMap.getOrElse(docName, 0.0) + (tfidfdoc * tfidfquery))
          }
        }
      }
      queryVectorNorm = math.sqrt(queryVectorNorm)
      scoreMap = scoreMap.map(idDist => (idDist._1, idDist._2 / (queryVectorNorm * docVectorNorms(idDist._1))))
      result = scoreMap.toSeq.sortWith(_._2 > _._2).take(scoringOptions.nDocsToBeReturned)
    }
    //If we don't want to use index (use Index == false)
    //  or the index is not available
    // => need to iterate through tipster stream
    else {
      
      println("Analyzing query: " + query)
      
      //we start doing a first iteration over all documents
      //to get the document frequencies of all the terms in the query
      //in order to be able to compute the tf-idf score in the second round
      val docs = new TipsterStreamSmart(path, options)
      var documentFrequencies = Map[String, Int]()
      var nDocs = 0
      var t = Timer(500, heapInfo = true)
      for (doc <- docs.stream) {
        t.progress(s"${doc.ID}, ${doc.name}") 
        for (tf <- doc.termFrequencies) {
          val term = tf._1
          val freq = tf._2  
          documentFrequencies += term -> (documentFrequencies.getOrElse(term, 0) + 1)
        }
        nDocs += 1
      }
      var queryVectorNorm = 0.0
      queryTermFrequency.foreach{
        queryTerm => 
          val tfquery = queryTerm._2
          val df = documentFrequencies.getOrElse(queryTerm._1, 0)
          val idf = if(df > 0) math.log(nDocs.toDouble / df.toDouble) else 0.0
          val tfidfquery = math.log(1 + tfquery) * idf
          queryVectorNorm += (tfidfquery * tfidfquery)
      }
      queryVectorNorm = math.sqrt(queryVectorNorm)
      
      //in the secound iteration over all documents
      //we compute the score for each document 
      //and keep a list of the top-n ranked documents
      t = Timer(500, heapInfo = true)
      for (doc <- docs.stream) {
        t.progress(s"${doc.ID}, ${doc.name}") 
        val documentLength = doc.getDocumentLength
        var docScore = 0.0
        var docVectorNorm = 0.0     
        for (tf <- doc.termFrequencies) {
          val term = tf._1
          val tfdoc = tf._2
          val df = documentFrequencies.getOrElse(term, 0)
          val idf = math.log(nDocs.toDouble / df.toDouble)
          val tfidfdoc = math.log(1 + tfdoc) * idf
          docVectorNorm += (tfidfdoc * tfidfdoc)
          if(query.contains(term)) {
            val tfquery = queryTermFrequency(term)
            val tfidfquery = math.log(1 + tfquery) * idf
            docScore += (tfidfdoc * tfidfquery)
          }
        }      
        if(docScore > 0.0) {
          docVectorNorm = math.sqrt(docVectorNorm)
          docScore = docScore / (queryVectorNorm * docVectorNorm)
          result = result ++ Seq((doc.name, docScore)) 
          if(result.size > scoringOptions.nDocsToBeReturned) {
            result = result.sortWith { case((d1, s1), (d2, s2)) => if (s1 > s2) true else if (s1 == s2 && d1 < d2) true else false }.take(scoringOptions.nDocsToBeReturned)
          }
        }
      }
      result = result.sortWith { case((d1, s1), (d2, s2)) => if (s1 > s2) true else if (s1 == s2 && d1 < d2) true else false }.take(scoringOptions.nDocsToBeReturned)
    }
    result
  }
  
  def computeTfIdfScoresForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)] = {    
    var scoreMap = Map[String, Double]()
    var result = Seq[(String, Double)]()
    val queryTermFrequency = collection.mutable.Map[String, Int]()
    query.foreach(t => {
      queryTermFrequency += t -> (1 + queryTermFrequency.getOrElse(t, 0))
    })
    
    //use index if useIndex == true and index available
    if(useIndex && idx != null) {
      println("Analyzing query: " + query)
      query.foreach{
        queryTerm => 
          val idxList = idx.index.getOrElse(queryTerm, List()) 
          if(idxList.nonEmpty) {
            val df = idxList.size
            val idf = math.log(nDocs.toDouble / df.toDouble)
            idxList.foreach{
              index => 
                val docName = idx.getDocName(index.id)
                val tf = index.freq
               
                val augmentedTf = (0.5 + (0.5 * (tf.toDouble / docMaxFrequency(docName))))
                val tfidf = augmentedTf * idf
                
                /*val tfNorm = (tf.toDouble / docMaxFrequency(docName).toDouble)
                val tfidf = math.log(1 + tfNorm) * idf*/
                
                //val tfidf = math.log(1 + tf) * idf
                
                scoreMap += docName -> (scoreMap.getOrElse(docName, 0.0) + tfidf) 
            }
        }      
      }
      //result = scoreMap.toSeq.sortWith(_._2 > _._2).take(scoringOptions.nDocsToBeReturned)
      result = scoreMap.toSeq.sortWith { case((d1, s1), (d2, s2)) => if (s1 > s2) true else if (s1 == s2 && d1 < d2) true else false }.take(scoringOptions.nDocsToBeReturned)
    }
    //If we don't want to use index (use Index == false)
    //  or the index is not available
    // => need to iterate through tipster stream
    else {
      
      println("Analyzing query: " + query)
      
      //we start doing a first iteration over all documents
      //to get the document frequencies of all the terms in the query
      //in order to be able to compute the tf-idf score in the second round
      val docs = new TipsterStreamSmart(path, options)
      var documentFrequencies = Map[String, Int]()
      var nDocs = 0
      var t = Timer(500, heapInfo = true)
      for (doc <- docs.stream) {
        t.progress(s"${doc.ID}, ${doc.name}") 
        for (tf <- doc.termFrequencies.filter(tf => (query.contains(tf._1)))) {
          val term = tf._1
          val freq = tf._2  
          documentFrequencies += term -> (documentFrequencies.getOrElse(term, 0) + 1)
        }
        nDocs += 1
      }
      
      //in the secound iteration over all documents
      //we compute the score for each document 
      //and keep a list of the top-n ranked documents
      t = Timer(500, heapInfo = true)
      for (doc <- docs.stream) {
        t.progress(s"${doc.ID}, ${doc.name}") 
        val documentLength = doc.getDocumentLength
        var docScore = 0.0
        var maxTermFreqency = 0
        if(doc.termFrequencies.length > 0) maxTermFreqency = doc.termFrequencies.map(tf => tf._2).max
        for (tf <- doc.termFrequencies.filter(tf => (query.contains(tf._1)))) {
          val term = tf._1
          val freq = tf._2
          val df = documentFrequencies.getOrElse(term, 0)
          val idf = math.log(nDocs.toDouble / df.toDouble)
          val augmentedTf = (0.5 + (0.5 * (freq.toDouble / maxTermFreqency.toDouble)))
          val tfidf = augmentedTf * idf
          docScore += (tfidf * queryTermFrequency.getOrElse(term, 0).toDouble)
        }
        if(docScore > 0.0) {
          result = result ++ Seq((doc.name, docScore)) 
          if(result.size > scoringOptions.nDocsToBeReturned) {
            //result = result.sortWith(_._2 > _._2).take(scoringOptions.nDocsToBeReturned)
            result = result.sortWith { case((d1, s1), (d2, s2)) => if (s1 > s2) true else if (s1 == s2 && d1 < d2) true else false }.take(scoringOptions.nDocsToBeReturned)
          }
        }
      }
      result = result.sortWith { case((d1, s1), (d2, s2)) => if (s1 > s2) true else if (s1 == s2 && d1 < d2) true else false }.take(scoringOptions.nDocsToBeReturned)
    }
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
     
<<<<<<< Updated upstream
    val options = TipsterOptions(maxDocs = 100000, chopping = -1, ngramSize = 0, useSynonyms = false)
=======
    val options = TipsterOptions(maxDocs = 100000, chopping = -1, ngramSize = 0, useSynonyms = true, splitLong = true)
>>>>>>> Stashed changes
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
 
    val useIndex = true
    val termModel = new TermBasedModel(persistentIndex, docPath, options, useIndex)
     
    val nDocsToBeReturned = 100
    val scoringMethod = "TFIDF"
    //val scoringMethod = "COSINESIMILARITY"
    val scoringOptions = ScoringModelOptions(
        nDocsToBeReturned = nDocsToBeReturned,
        scoringMethod = scoringMethod)
    
    /*val fname = infiles.DocPath + "AP880212-0006"
    val parse = new TipsterParseSmart(ch.ethz.dal.tinyir.io.DocStream.getStream(fname), options)
    var sampleQuery = TipsterParseSmart.tokenize(parse.content, options)
    sampleQuery = sampleQuery ++ List("adfdasfadad")
    var result = termModel.computeScoreForQuery(sampleQuery, scoringOptions)
    println(result)*/
    
    /*val sampleQuery = TipsterParseSmart.tokenize("along aircraft dead whereabout adasdsdfasd", options)
    println(sampleQuery) 
    var result = termModel.computeScoreForQuery(sampleQuery, scoringOptions)
    println(result)
    println(result.size)*/
    
    var scores = termModel.getScores(queryParse.queries, scoringOptions)
    println(scores)
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