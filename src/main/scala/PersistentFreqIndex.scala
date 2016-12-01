/**
  * @author Michael
  * @author mmgreiner
  *
  */

import ch.ethz.dal.tinyir.indexing._
import java.io.File

import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._

import scala.collection.mutable


case class FreqPosting(id: Int, name: String, freq: Int) extends Ordered[FreqPosting] {
  def compare(that: FreqPosting) = this.id compare that.id
}

class PersistentFreqIndex(path: String, dbPath: String,
                          forceIndexRecreation: Boolean,
                          options: TipsterOptions = TipsterOptions()) extends InvertedIndex[FreqResult] {

  val index: mutable.Map[String, List[FreqPosting]] = {
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
    makeIndexStructurePersistent(dbPath, this.index)
  }

  println("index contains term frequencies from totally " + getAmountOfDocsInIndex() + " documents")

  def createIndex(path: String, options: TipsterOptions = TipsterOptions()): mutable.Map[String, List[FreqPosting]] = {
    val docs = new TipsterStreamSmart(path, options)
    val index = mutable.Map[String, List[FreqPosting]]()
    val t = Timer(100, heapInfo = true)
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
    index
  }

  def makeIndexStructurePersistent(dbPath: String, index: mutable.Map[String, List[FreqPosting]]) = {
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
    } finally {
      db.close()
    }
    println("inverted index stored in db")
  }


  /**
    * a pattern that matches the key-values stored in the database. See getDB how to use it.
    */
  val patEntry = "\\((.+),(.+),(.+)\\)".r

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
      case patEntry(id, name, freq) => new FreqPosting(id.toInt, name, freq.toInt)
    }.toList
  }


  def recreateIndexFromDisk(): mutable.Map[String, List[FreqPosting]] = {
    println("recreating inverted index from db started")
    var index = mutable.Map[String, List[FreqPosting]]()
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(new File(dbPath), options)
    val iterator = db.iterator()
    try {
      iterator.seekToFirst()
      while (iterator.hasNext()) {
        val key = asString(iterator.peekNext().getKey())
        val value = asString(iterator.peekNext().getValue())
        val postingStringList = value.split(" ")
        val postings = postingStringList.map(
          psl => FreqPosting(
            psl.substring(1, psl.length - 1).split(",")(0).toInt,
            psl.substring(1, psl.length - 1).split(",")(1),
            psl.substring(1, psl.length - 1).split(",")(2).toInt)).toList
        index += key -> postings
        iterator.next()
      }
    } finally {
      // Make sure you close the iterator to avoid resource leaks.
      iterator.close()
    }
    println("recreating inverted index from db finished")
    index
  }

  override def results(term: String): List[FreqResult] =
    index.getOrElse(term, Nil).map(p => FreqResult(p.id, List(p.freq)))


  def getAmountOfDocsInIndex(): Int = {
    this.index.flatMap(index => index._2.map(fp => fp.id)).toSet.size
  }

  def getDocIdsInIndex(): Set[Int] = {
    this.index.flatMap(index => index._2.map(fp => fp.id)).toSet
  }

  def getDocNamesInIndex(): Set[String] = {
    this.index.flatMap(index => index._2.map(fp => fp.name)).toSet
  }
}


object PersistentFreqIndex {
  def main(args: Array[String]): Unit = {

    val options = TipsterOptions(maxDocs = 100000)
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database
    val forceIndexRecreation = true

    val persistentIndex = new PersistentFreqIndex(docPath, dbPath, forceIndexRecreation, options)
  }


}