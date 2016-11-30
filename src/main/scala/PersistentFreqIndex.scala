import ch.ethz.dal.tinyir.indexing._
import ch.ethz.dal.tinyir.processing._
import ch.ethz.dal.tinyir.io._
import java.io.File

import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import java.time.LocalDateTime
import java.time.Duration

import scala.collection.mutable.HashMap
import scala.util.Try

case class FreqPosting(id: Int, name: String, freq: Int) extends Ordered[FreqPosting] {
  def compare(that: FreqPosting) = this.id compare that.id
}

//class PersistentFreqIndex(docPath : String, nDocs : Int, dbPath : String, forceIndexRecreation : Boolean) extends InvertedIndex[FreqResult] {
class PersistentFreqIndex(docs : Stream[XMLDocument], dbPath : String, forceIndexRecreation : Boolean, batchSize : Int, appendBatchesToDB : Boolean) extends InvertedIndex[FreqResult] {


  type PostList = List[FreqPosting]


    val index: Map[String, PostList] = {
      //if db does not exist or if we force a recreation
      if (forceIndexRecreation) {
        println("creation of inverted index started")
        var docsProcessed = 0
        var overallMap = Map[String, List[FreqPosting]]()
        while (docsProcessed < docs.size) {
          val docsSub = docs.slice(docsProcessed, docsProcessed + batchSize)
          val groupedTuples = postings(docsSub).groupBy(_.term)
          val result = groupedTuples.mapValues(_.map(tfT => FreqPosting(tfT.doc, "TODO-Michael", tfT.count)).sorted)
          if (!appendBatchesToDB) {
            overallMap = overallMap ++ result.map {
              case (term, listFP) => term -> (listFP ++ overallMap.getOrElse(term, List()))
            }
          }
          docsProcessed += batchSize
          println("docs processed: " + docsProcessed)
          if (appendBatchesToDB) {
            if (batchSize == docsProcessed) {
              makeIndexStructurePersistent(dbPath, result)
            } else {
              appendIndexStructureToDB(dbPath, result)
            }
          }
        }

        println("inverted index created")
        if (!appendBatchesToDB) {
          makeIndexStructurePersistent(dbPath, overallMap)
        }
        overallMap

      }
      //otherwise: load db content into memory
      else {
        recreateIndexFromDisk()
      }
    }
    //println(index)


    //val namesMap = new HashMap[Int, String]()
    //var docNames = new TipsterStreamSmart(docPath, "", true, true, nDocs).stream.map(doc => doc.name).toList
    //docNames.foreach { dn => namesMap.put(dn.hashCode(), dn) }

    case class TfTuple(term: String, doc: Int, count: Int)
    private def postings(s: Stream[Document]): List[TfTuple] =
    s.flatMap(d => d.tokens.groupBy(identity)
      .map { case (tk, lst) => TfTuple(tk, d.ID, lst.length) }).toList

    override def results(term: String): List[FreqResult] =
    index.getOrElse(term, Nil).map(p => FreqResult(p.id, List(p.freq)))

    def makeIndexStructurePersistent(dbPath: String, index: Map[String, PostList]) = {
      println("storing inverted index in db started")
      val options = new Options()
      var dbFile = new File(dbPath)
      factory.destroy(dbFile, options)
      dbFile = new File(dbPath + "dbWithIndexOf" + docs.size + "Docs")
      options.createIfMissing(true)
      val db = factory.open(dbFile, options)
      try {
        index.foreach {
          case (d, lst) =>
            db.put(bytes(d), bytes(lst.mkString(" ").replace("FreqPosting", "")))
        }
      } finally {
        db.close()
      }
      println("inverted index stored in db")
    }

    def appendIndexStructureToDB(dbPath: String, index: Map[String, PostList]) = {
      println("appending inverted index to db started")
      //println(index)
      val options = new Options()
      options.createIfMissing(true)
      val db = factory.open(new File(dbPath), options)
      try {
        index.foreach {
          index =>
            //println("term: " + index._1)
            //println("list of freqPosting: " + index._2)
            var value = asString(db.get(bytes(index._1)))
            if (value != null) {
              val postingStringList = value.split(" ")
              //println("value in db: " + value)
              var postings = postingStringList.map(
                psl => FreqPosting(
                  psl.substring(1, psl.length - 1).split(",")(0).toInt,
                  "TODO-Michael",
                  psl.substring(1, psl.length - 1).split(",")(1).toInt)).toList
              //println("already in db: " + postings)
              postings = postings ++ index._2
              //println("new in db: " + postings)
              db.put(bytes(index._1), bytes(postings.mkString(" ").replace("FreqPosting", "")))
            } else {
              db.put(bytes(index._1), bytes(index._2.mkString(" ").replace("FreqPosting", "")))
            }
        }
      } catch {
        case e: Exception => println("exception caught: " + e)
      } finally {
        db.close()
      }
      println("inverted index appended to db")
    }

    def recreateIndexFromDisk(): Map[String, PostList] = {
      println("recreating inverted index from db started")
      var index = Map[String, PostList]()
      val options = new Options()
      options.createIfMissing(true)
      val db = factory.open(new File(dbPath), options)
      val iterator = db.iterator()
      try {
        iterator.seekToFirst()
        while (iterator.hasNext()) {
          val key = asString(iterator.peekNext().getKey())
          val value = asString(iterator.peekNext().getValue())
          //println(key + " / " + value)
          val postingStringList = value.split(" ")
          val postings = postingStringList.map(
            psl => FreqPosting(
              psl.substring(1, psl.length - 1).split(",")(0).toInt,
              "TODO-Michael",
              psl.substring(1, psl.length - 1).split(",")(1).toInt)).toList
          index += key -> postings
          //println(postings)
          iterator.next()
        }
      } finally {
        // Make sure you close the iterator to avoid resource leaks.
        iterator.close()
      }
      println("recreating inverted index from db finished")
      println(index)
      index
    }

    def getAmountOfDocsInIndex(): Int = {
      this.index.flatMap(index => index._2.map(fp => fp.id)).toSet.size
    }

    def getDocsInIndex(): Set[Int] = {
      this.index.flatMap(index => index._2.map(fp => fp.id)).toSet
    }
  }


object PersistentFreqIndex {
  def main(args: Array[String]): Unit = {

    val startTime = LocalDateTime.now()
    val nDocs = 10
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database

    createIndex(docPath)

    return

    val batchSize = 1
    val recomputeIndex = true
    val tipsterStream = new TipsterStreamSmart(docPath,
      numbers = true, stopWords = true, stemming = true, maxDocs = nDocs).stream
    val appendBatchesToDB = true
    var idx = new PersistentFreqIndex(tipsterStream, dbPath, recomputeIndex, batchSize, appendBatchesToDB)
    //var idx = new PersistentFreqIndex(docPath, nDocs, dbPath, recomputeIndex)    
    val endTime = LocalDateTime.now()
    val duration = Duration.between(startTime, endTime)
    println("Time needed: " + duration)
  }

  /**
    * test if it is possible to get the whole index into memory.
    * Create an index of Terms and tuples with the document id and the frequency inside that document
    *
    * @param path path to directory with zip file
    */
  def createIndex(path: String): Map[String, List[FreqPosting]] = {
    val docs = new TipsterStreamSmart(path, stopWords = true, stemming = true, numbers = true, chopping = 6)
    val index = collection.mutable.Map[String, List[FreqPosting]]()
    val t = Timer(500, heapInfo = true)
    for (doc <- docs.stream) {
      val id = doc.ID
      t.progress(s"$id, ${doc.name}")
      for (tf <- doc.termFrequencies) {
        val term = tf._1
        val freq = tf._2
        val fposts = FreqPosting(id, doc.name, freq) :: index.getOrElse(term, List[FreqPosting]())
        index += term -> fposts
      }
    }
    println(s"completed in ${t.elapsed()} secs")
    println(s"head: ${index.head}")
    println(s"size: ${index.size}")
    index.toMap
  }

}