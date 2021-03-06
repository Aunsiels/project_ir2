/**
  * @author Michael
  * @author mmgreiner
  *
  */

import ch.ethz.dal.tinyir.indexing._
import java.io.File
import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import scala.util.Success
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

case class FreqPosting(id: Int, freq: Int) extends Ordered[FreqPosting] {
  def compare(that: FreqPosting) = this.id compare that.id
}

class PersistentFreqIndex(path: String, dbPath: String,
                          forceIndexRecreation: Boolean,
                          options: TipsterOptions = TipsterOptions()) extends InvertedIndex[FreqResult] {

  var docHashMap = Map[Int, String]()
    
  val index: Map[String, List[FreqPosting]] = {
    //if db does not exist or if we force a recreation
    if (forceIndexRecreation) {
      createIndex(path, options)
    }
    //otherwise: load db content into memory
    else {
      recreateIndexFromDisk()
    }
  }

  if (forceIndexRecreation) {
    makeIndexStructurePersistent(dbPath, this.index, this.docHashMap)
  }

  println("index contains term frequencies from totally " + getAmountOfDocsInIndex() + " documents")

  def createIndex(path: String, options: TipsterOptions = TipsterOptions()): Map[String, List[FreqPosting]] = {
    val docs = new TipsterStreamSmart(path, options)
    
    val index = Map[String, List[FreqPosting]]()
    val t = Timer(500, heapInfo = true)
    for (doc <- docs.stream) {
      val id = doc.ID
      docHashMap += doc.ID -> doc.name
      t.progress(s"$id, ${doc.name}")

      for (tf <- doc.termFrequencies) {
        val term = tf._1
        val freq = tf._2
        val fposts = FreqPosting(id, freq) :: index.getOrElse(term, List[FreqPosting]())
        index += term -> fposts
      }
    }
    println(s"completed in ${t.elapsed()} secs")
    println(s"head: ${index.head}")
    println(s"size: ${index.size}")
    println(s" tokenizer split statistics: 'and': ${TipsterParseSmart.andCount}, " +
      s"'the': ${TipsterParseSmart.theCount}, long-words: ${TipsterParseSmart.splitCount}")
    index
  }

  /**
    * test different data structures for performance. It turns out that List and ListBuffer with prepend are the fastest.
    * @param path
    * @param options
    * @return
    */
  def createIndex2(path: String, options: TipsterOptions = TipsterOptions()): Map[String, List[FreqPosting]] = {
    val docs = new TipsterStreamSmart(path, options)
    val index = Map[String, ListBuffer[FreqPosting]]()
    println(s"*** ${index.getClass}")
    val t = Timer(heapInfo = true)
    for (doc <- docs.stream) {
      val id = doc.ID
      t.progress(s"$id, ${doc.name}")
      for (tf <- doc.termFrequencies) {
        val term = tf._1
        val freq = tf._2
        val fp = FreqPosting(id, freq)
        // ugly but fast
        try {
          index(term) += fp
        }
        catch {
          case ex: java.util.NoSuchElementException => index += term -> ListBuffer(fp)
        }
      }
    }
    println(s"completed in ${t.elapsed()} secs")
    println(s"head: ${index.head}")
    println(s"size: ${index.size}")
    index.map(x => x._1 -> x._2.toList)
  }

  def makeIndexStructurePersistent(dbPath: String, index: Map[String, List[FreqPosting]], docMap: Map[Int, String]) = {
    println("storing inverted index in db started")
    val options = new Options()
    var dbFile = new File(dbPath)
    factory.destroy(dbFile, options)
    dbFile = new File(dbPath)
    options.createIfMissing(true)
    val db = factory.open(dbFile, options)
    try {
      index.foreach {
        case (d, lst) =>
          db.put(bytes(d), bytes(lst.mkString(" ").replace("FreqPosting", "")))
      }
      db.put(bytes("ID_Hash"), bytes(docMap.mkString(",")))
    } finally {
      db.close()
    }
    println("inverted index stored in db")
  }



  /**
    * get the List[FreqPosting] from the database of the given key which is a term
    * @param key term
    * @param db levelDB must be opened
    * @return List[FreqPosting] as stored in the database, or emtpy List, if key is not found.
    */
  def getDB(key: String, db: DB): List[FreqPosting] = {
    val v = asString(db.get(bytes(key)))
    if (v == null) List[FreqPosting]()
    else v.split(" ").map {
      case PersistentFreqIndex.patEntry2(id, freq) => FreqPosting(id.toInt, freq.toInt)
      case PersistentFreqIndex.patEntry3(id, name, freq) => FreqPosting(id.toInt, freq.toInt)
    }.toList
  }



  def recreateIndexFromDisk(): Map[String, List[FreqPosting]] = {
    println(s"recreating inverted index from db $dbPath started")
    val index = Map[String, List[FreqPosting]]()
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(new File(dbPath), options)
    val iterator = db.iterator()
    try {
      iterator.seekToFirst()
      val t = Timer(step = 5000, heapInfo = true)

      while (iterator.hasNext()) {
        val key = asString(iterator.peekNext().getKey())
        t.progress(s"$key")
        if(key == "ID_Hash") {
          val value = asString(iterator.peekNext().getValue())
          val idHashStringList = value.split(",")
          idHashStringList.foreach {
            idHash =>
              val idHashArray = idHash.split(" -> ")
              val id = idHashArray(0)
              val hash = idHashArray(1)
              docHashMap += id.toInt -> hash
          }
        } else {
          val value = asString(iterator.peekNext().getValue())
          val postingStringList = value.split(" ")
          /*
          val postings = postingStringList.map(
            psl => FreqPosting(
              psl.substring(1, psl.length - 1).split(",")(0).toInt,
              psl.substring(1, psl.length - 1).split(",")(1).toInt)).toList
              */
          /**
            * take care of older version with 3 entries and new version that does not have the name
            */
          val postings = postingStringList.map {
            case PersistentFreqIndex.patEntry3(id, name, freq) =>
              docHashMap += id.toInt -> name
              FreqPosting(id.toInt, freq.toInt)
            case PersistentFreqIndex.patEntry2(id, freq) =>
              FreqPosting(id.toInt, freq.toInt)
          }.toList
          index += key -> postings
        }
        iterator.next()
      }
    } finally {
      // Make sure you close the iterator to avoid resource leaks.
      iterator.close()
      db.close()
    }
    println("recreating inverted index from db finished")
    index
  }

  override def results(term: String): List[FreqResult] =
    index.getOrElse(term, Nil).map(p => FreqResult(p.id, List(p.freq)))


  def getAmountOfDocsInIndex(): Int = {
    this.docHashMap.size
  }

  def getDocIdsInIndex(): Set[Int] = {
    this.index.flatMap(index => index._2.map(fp => fp.id)).toSet
  }

  def getDocNamesInIndex(): Set[String] = {
    this.docHashMap.values.toSet
  }
  
  def getDocName(id: Int): String = {
    this.docHashMap.getOrElse(id, "")
  }
}


object PersistentFreqIndex {
  def main(args: Array[String]): Unit = {

    val options = TipsterOptions(maxDocs = 100000, chopping = -1, splitLong = true)
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database
    val forceIndexRecreation = true

    val persistentIndex = new PersistentFreqIndex(docPath, dbPath, forceIndexRecreation, options)
  }

  /**
    * a pattern that matches the key-values stored in the database. See getDB how to use it.
    */
  val patEntry2 = "\\((.+),(.+)\\)".r
  val patEntry3 = "\\((.+),(.+),(.+)\\)".r



}