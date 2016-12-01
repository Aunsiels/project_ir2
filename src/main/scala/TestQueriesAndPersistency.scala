/**
  * Created by mmgreiner on 01.12.16.
  */

import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import java.io.File

import util.Try

object TestQueriesAndPersistency {

  var db: DB = null

  def openDB(fname: String): Unit = {
    val options = new Options
    db = factory.open(new File(fname), options)
  }

  val patEntry = "\\((.+),(.+),(.+)\\)".r

  def getDB(key: String, db: DB = db): List[FreqPosting] = {
    val v = asString(db.get(bytes(key)))
    if (v == null) List[FreqPosting]()
    else v.split(" ").map {
      case patEntry(id, name, freq) => new FreqPosting(id.toInt, name, freq.toInt)
    }.toList
  }


  def main(args: Array[String]): Unit = {
    val inf = InputFiles(args)

    val options = TipsterOptions(maxDocs = 40000)
    //val fi = new PersistentFreqIndex(inf.DocPath, inf.Database, false, options)
    val queries = QueryParse(inf.Queries, options)

    try {
      openDB(inf.Database)
      for (q <- queries.queries) {
        println(s"query ${q._1} ${q._2} ****")
        val tokenFreqs = q._2.map(x => {   // query tokens
          val entries = getDB(x)
          entries.length
        })
        val tf = tokenFreqs.mkString(" ")
        println(s"               $tf")
      }
    }
      catch {
        case e: DBException => println(e.printStackTrace)
      }
    finally  {
      db.close
    }
    println(s"survived")
  }
}
