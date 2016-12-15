/**
  * Created by mmgreiner on 01.12.16.
  */

import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
//import java.io.File
import java.io._
import java.util.Calendar

/**
  * Do some test which access levelDB directly to get inverted index, and compare results to given relevance judgements.
  */
object TestQueriesAndPersistency {

  var db: DB = _

  def openDB(fname: String): Unit = {
    val options = new Options
    db = factory.open(new File(fname), options)
  }

  val patEntry = "\\((.+),(.+),(.+)\\)".r

  def getDB(key: String, db: DB = db): List[FreqPosting] = {
    val v = asString(db.get(bytes(key)))
    if (v == null) List[FreqPosting]()
    else v.split(" ").map {
      //case patEntry(id, name, freq) => FreqPosting(id.toInt, name, freq.toInt)
      case patEntry(id, _, freq) => FreqPosting(id.toInt, freq.toInt)
    }.toList
  }

  def testTokenizer(inf: InputFiles): Unit = {
    println(s"reading ${inf.DocPath}")
    val options = TipsterOptions(splitLong = true)
    val t = Timer()
    for (doc <- new TipsterStreamSmart(inf.DocPath, options).stream) {
      val tf = doc.termFrequencies
      t.progress(s"${doc.name} ${doc.title}")
    }
    println(s"'and' ${TipsterParseSmart.andCount}, 'the' ${TipsterParseSmart.theCount}, " +
      s"xxx-words ${TipsterParseSmart.xxxCount}, long-words ${TipsterParseSmart.splitCount}")
  }

  def testModel(inf: InputFiles) = {
    val options = TipsterOptions(maxDocs = 100000, chopping = -1, ngramSize = 0, useSynonyms = false, splitLong = true)
    val persistentIndex = new PersistentFreqIndex(inf.DocPath, inf.Database, false, options)

    // val queryParse = QueryParse(queryPath, options)
    val relevance = RelevanceJudgementParse(inf.Relevance)

    val termModel = new TermBasedModel(persistentIndex, inf.DocPath, options, true)

    val scoringOptions = ScoringModelOptions(nDocsToBeReturned = 100, scoringMethod = "TFIDF")

    val relevanceQueries = relevance.docs.map(x => x._1.toString -> x._2)
    val scores = termModel.getScores(relevanceQueries, scoringOptions)
    termModel.convertScoresToListOfDocNames(scores).foreach{
      tfIdfScore =>
        println("Score for query: " + tfIdfScore._1)
        println("relevant docs: " + relevance.docs(tfIdfScore._1))
        println("proposed docs: " + tfIdfScore._2)
        val stat = Evaluation.getStat(tfIdfScore._2, relevance.docs(tfIdfScore._1), 1)
        println(stat)
    }

  }

  /**
    * Test the parser with various options
    * @param inf
    */
  def testTipster(inf: InputFiles): Unit = {
    val date = Timer.now()

    List(TipsterOptions(stopWords = false, stemming = false, useSynonyms = false, splitLong = false),
      TipsterOptions(stopWords = true, stemming = false, useSynonyms = false, splitLong = false),
      TipsterOptions(stopWords = true, stemming = false, useSynonyms = false, splitLong = true),
      TipsterOptions(stopWords = true, stemming = true, useSynonyms = false, splitLong = true),
      TipsterOptions(stopWords = true, stemming = true, useSynonyms = false, splitLong = false)
    ).foreach(opt => {
      println(s"stop ${opt.stopWords}, stem ${opt.stemming}, syn ${opt.useSynonyms}, split ${opt.splitLong}")
      val t = Timer(step = 2000, heapInfo = true)
      val termFreq = collection.mutable.Map[String, Int]()
      var docs = new TipsterStreamSmart(inf.DocPath, opt)
      for (doc <- docs.stream) {
        t.progress(s"${doc.name} ${doc.title.slice(0, 60)}")
        val tf = doc.termFrequencies
        tf.foreach(t => termFreq += t._1 -> (t._2 + termFreq.getOrElse(t._1, 0)))
      }
      // TODO sort by value descending
      println(s"total terms ${termFreq.size}")
      var sorted = termFreq.toSeq.sortWith(_._2 > _._2)
      val elapsed = t.elapsed()

      println(s"time: $elapsed top: ${sorted.slice(0,20)}")

      // Write to file
      val fname = s"termFreq_${date}_${opt.stopWords}_${opt.stemming}_${opt.useSynonyms}_${opt.splitLong}.txt"
      println(s"writing $fname")
      val pw = new PrintWriter(new File(fname))
      pw.println(s"# $fname ${Calendar.getInstance().toString} total ${termFreq.size} elapsed $elapsed")
      sorted.foreach(t => pw.println(s"${t._1} ${t._2}"))
      pw.close()
      println("completed")

      sorted = null
      docs = null
      termFreq.clear
      System.gc()
    })



  }

  def main(args: Array[String]): Unit = {
    val inf = InputFiles(args)

    // testModel(inf)

    // testTokenizer(inf)

    testTipster(inf)

    return

    val options = TipsterOptions(maxDocs = 40000, splitLong = true)

    //val fi = new PersistentFreqIndex(inf.DocPath, inf.Database, false, options)
    val queries = QueryParse(inf.Queries, options)
    val relDocs = RelevanceJudgementParse(inf.Relevance)

    var orphanTokens = List[String]()
    val wordnet = Wordnet()

    try {
      openDB(inf.Database)
      for (q <- queries.queries) {
        println(s"query ${q._1} ${q._2} ****")
        val tokenFreqs = q._2.map(x => {   // query tokens
          val entries = getDB(x)
          if (entries.isEmpty) orphanTokens = x :: orphanTokens
          entries.length
        })
        val tf = tokenFreqs.mkString(" ")
        println(s"               $tf")
      }

      println(s"tokens not found: $orphanTokens")
    }
      catch {
        case e: DBException => println(e.printStackTrace())
      }
    finally  {
      db.close()
    }
    println(s"survived")
  }

}
