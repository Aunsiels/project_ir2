import ch.ethz.dal.tinyir.indexing._
import ch.ethz.dal.tinyir.processing._
import ch.ethz.dal.tinyir.io._
import java.io.File
import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
import scala.util.Try

import scala.collection.mutable.Map


case class FreqPosting(id: Int, name: String, freq: Int) extends Ordered[FreqPosting] {
  def compare(that: FreqPosting) = this.id compare that.id
}

class PersistentFreqIndex(path : String, dbPath : String, maxDocs: Int, forceIndexRecreation : Boolean) extends InvertedIndex[FreqResult] {

    val index: Map[String, List[FreqPosting]] = {
      //if db does not exist or if we force a recreation
      if (forceIndexRecreation) {
        createIndex(path, maxDocs)
      }
      //otherwise: load db content into memory
      else {
        recreateIndexFromDisk()
      }
    }
    
    if(forceIndexRecreation) { 
      makeIndexStructurePersistent(dbPath, this.index)
    }
        
    println("index contains term frequencies from totally " + getAmountOfDocsInIndex() + " documents")
    
    def createIndex(path: String, maxDocs: Int): Map[String, List[FreqPosting]] = {
      val docs = new TipsterStreamSmart(path, stopWords = true, stemming = true, numbers = true, chopping = 6, maxDocs = maxDocs)
      val index = Map[String, List[FreqPosting]]()
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
      index
    }
  
    def makeIndexStructurePersistent(dbPath: String, index: Map[String, List[FreqPosting]]) = {
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

    
    def recreateIndexFromDisk(): Map[String, List[FreqPosting]] = {
      println("recreating inverted index from db started")
      var index = Map[String, List[FreqPosting]]()
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

    val nDocs = 10000
    val infiles = InputFiles(args)
    val docPath = infiles.DocPath
    val dbPath = infiles.Database
    val forceIndexRecreation = true

    val persistentIndex = new PersistentFreqIndex(docPath, dbPath, nDocs, forceIndexRecreation)
     
    return
  }  

}