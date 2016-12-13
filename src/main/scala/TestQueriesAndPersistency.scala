/**
  * Created by mmgreiner on 01.12.16.
  */

import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import java.io.File

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

  def main(args: Array[String]): Unit = {
    val inf = InputFiles(args)

    testModel(inf)

    testTokenizer(inf)
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
