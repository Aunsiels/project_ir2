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


  def main(args: Array[String]): Unit = {
    val inf = InputFiles(args)

    val options = TipsterOptions(maxDocs = 40000)
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
      TestOne(54, "satellit", relDocs)
    }
      catch {
        case e: DBException => println(e.printStackTrace())
      }
    finally  {
      db.close()
    }
    println(s"survived")
  }

  def TestOne(queryId: Int, token: String, relDocs: RelevanceJudgementParse): Unit = {
    val docs89 = getDB(token).sorted
    val relevance89 = relDocs.docs((queryId)).toSet

    /*val docsSet = docs89.map(x => x.name).toSet
    val diff2 = relevance89 -- docsSet
    val diff1 = docsSet -- relevance89
    val inter = docsSet intersect relevance89
    println(s"relevant ${relevance89.size}, docs ${docsSet.size}")
    println(s"docs intersect rel ${inter.size} $inter")
    println(s"docs - rel ${diff1.size} $diff1")
    println(s"rel - docs ${diff2.size} $diff2")*/

  }
}
