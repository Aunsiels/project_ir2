
import ch.ethz.dal.tinyir.io.TipsterStream
import java.io.{FileOutputStream, PrintWriter}
import scala.collection.JavaConversions._

class TipsterStreamSmart(val path: String,
                         val numbers: Boolean = true,
                         val stopWords: Boolean = true,
                         val stemming: Boolean = true,
                         val chopping: Int = -1,
                         val maxDocs: Integer = Int.MaxValue
                         ) extends TipsterStream(path, "") {

  override def stream : Stream[TipsterParseSmart] =
    unparsed.stream.slice(0, maxDocs).map(is => new TipsterParseSmart(is, reduceNumbers = numbers,
      reduceStopWords = stopWords, stemming = stemming, chopping = chopping))


  /**
    * compute the term frequency of all documents in this stream
    * @param progress text to println during progress
    * @return
    */
  def docFrequencies(progress: Boolean = false): Map[String, Int] = {
    val df = collection.mutable.Map[String, Int]()
    val t = Timer(step = 500, heapInfo = true)
    for (doc <- stream) {
      val h = doc.termFrequencies.headOption
      if (progress) t.progress(s"${doc.ID}, ${doc.name}, $h")
    }
    println(s"completed in ${t.elapsed()} secs")
    df.toMap
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