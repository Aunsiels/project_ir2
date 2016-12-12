/**
  * @author mmgreiner
  */

import ch.ethz.dal.tinyir.io.TipsterStream
import java.io.{FileOutputStream, PrintWriter}
import scala.collection.JavaConversions._

/**
  * Class to create a stream to all XML documents. The tokenizer has various options
  * @param path folder with zip file containing XML documents
  * @param numbers if true, tokenizer reduces numbers
  * @param stopWords if true, stop words are eliminated
  * @param stemming if true, Porter Stemmer is applied
  * @param chopping if > 0, all tokens are chopped to this length
  * @param maxDocs max number of documents handled in stream
  */
class TipsterStreamSmart(val path: String,
                         val numbers: Boolean = true,
                         val stopWords: Boolean = true,
                         val stemming: Boolean = true,
                         val chopping: Int = -1,
                         val maxDocs: Integer = Int.MaxValue,
                         val ngramSize: Integer = 0,
                         val splitLong: Boolean = true
                         ) extends TipsterStream(path, "") {

  def this(path: String, options: TipsterOptions) =
    this(path = path,
      numbers = options.numbers,
      stopWords = options.stopWords,
      stemming = options.stemming,
      chopping = options.chopping,
      maxDocs = options.maxDocs,
      ngramSize = options.ngramSize,
      splitLong = options.splitLong)


  override def stream : Stream[TipsterParseSmart] =
    unparsed.stream.slice(0, maxDocs).map(is =>
      new TipsterParseSmart(is,
        reduceNumbers = numbers,
        reduceStopWords = stopWords,
        stemming = stemming,
        chopping = chopping,
        ngramSize = ngramSize,
        splitLong = splitLong))


  /**
    * compute the term frequency of all documents in this stream
    * @param progress text to println during progress
    * @return
    */
  private def docFrequencies(progress: Boolean = false): Map[String, Int] = {
    val df = collection.mutable.Map[String, Int]()
    val t = Timer(step = 500, heapInfo = true)
    for (doc <- stream) {
      val h = doc.termFrequencies.headOption
      if (progress) t.progress(s"${doc.ID}, ${doc.name}, $h")
    }
    println(s"completed in ${t.elapsed()} secs")
    df.toMap
  }
  
  def collectionFrequencies(): collection.mutable.Map[String, Int] = {
    val t = Timer(500, heapInfo = true)
    var cf = collection.mutable.Map[String, Int]()
    for (doc <- this.stream) {
      t.progress(s"$doc.id, ${doc.name}")
      for(tf <- doc.termFrequencies) {
        cf += tf._1 -> (cf.getOrElse(tf._1, 0) + tf._2)
      }
    }
    println(s"completed in ${t.elapsed()} secs")
    cf
  }

}

object TipsterStreamSmart {

  def main(args: Array[String]): Unit = {
    val path = InputFiles(args).DocPath
    println(s"Testing ${TipsterParseSmart.getClass.getName} with $path")
    val stream = new TipsterStreamSmart(path)
    val t = Timer()
    val df = stream.docFrequencies(true)
    println(s"elapsed for frequencies ${t.elapsed()}, number of tokens ${df.size}")
    val sorted = df.toSeq.sortWith(_._2 > _._2)
    println(s"top frequencies ${sorted.slice(0, 20)}")
    println(s"ID hashes ${TipsterParseSmart.nameHash.slice(0,20)}")

  }
}