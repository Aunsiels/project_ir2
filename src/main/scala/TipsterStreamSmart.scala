
import ch.ethz.dal.tinyir.io.TipsterStream
import java.io.{FileOutputStream, PrintWriter}
import scala.collection.JavaConversions._

class TipsterStreamSmart(path: String, 
                         ext: String = "",
                         stopWords: Boolean = true,
                         stemming: Boolean = true,
                         maxDocs: Integer = Int.MaxValue,
                         frequencies: Boolean = false) extends TipsterStream(path, ext) {

  override def stream : Stream[TipsterParseSmart] =
    unparsed.stream.slice(0, maxDocs).map(is => new TipsterParseSmart(is, stopWords, stemming))


  /**
    * compute the term frequency of all documents in this stream
    * @param progress
    * @return
    */
  def docFrequencies(progress: Boolean = false): Map[String, Int] = {
    val df = collection.mutable.Map[String, Int]()
    val t = Timer(step = 500, heapInfo = true)
    for (doc <- stream) {
      if (progress) t.progress(s"${doc.ID}, ${doc.name}")
      df ++= doc.tokens.distinct.map(tok => tok -> (1 + df.getOrElse(tok, 0)))
    }
    println(s"completed in ${t.elapsed()} secs")
    df.toMap
  }

}

object TipsterStreamSmart {

  def main(args: Array[String]): Unit = {
    val path = InputFiles(args).DocPath
    println(s"Testing ${TipsterParseSmart.getClass.getName} with $path")
    val stream = new TipsterStreamSmart(path, frequencies = true)
    val t = Timer()
    val df = stream.docFrequencies(true)
    println(s"elapsed for frequencies ${t.elapsed()}, number of tokens ${df.size}")
    val sorted = df.toSeq.sortWith(_._2 > _._2)
    println(s"top frequencies ${sorted.slice(0, 20)}")
    println(s"ID hashes ${TipsterParseSmart.nameHash.slice(0,20)}")

  }
}