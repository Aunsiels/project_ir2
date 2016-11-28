/**
  * Created by mmgreiner on 26.11.16.
  */

/**
  * Class to test the performance of TipsterParseSmart
  */

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument

object PerformanceParseSmart {
  def main(args: Array[String]): Unit = {

    val bare = false
    val inf = InputFiles(args)
    println(s"working on ${inf.DocPath}")

    if (bare) {
      val t1 = new Timer()
      System.gc()
      println(s"dumb Stream...free ${Timer.freeMB()} MB, max ${Timer.totalMB()} MB")
      var dumbStream = new TipsterStream(inf.DocPath)
      val names = dumbStream.stream.map(x => (x.title, x.ID))
      val len = dumbStream.length
      println(s"dumb stream, size $len in ${t1.elapsed()} sec, free ${Timer.freeMB()} MB")

      val df1 = doc_freq(dumbStream.stream, progress = true)
      val sorted = df1.toSeq.sortWith(_._2 > _._2)
      println(s"size: ${sorted.size}, top: ${sorted.slice(0, 20)}")

      dumbStream = null
    }
    System.gc

    println(s"smart/slow Stream...free ${Timer.freeMB()} MB, max ${Timer.totalMB()} MB")
    val t3 = Timer()
    val inp = InputFiles(args)

    val smartStream = new TipsterStreamSmart(inp.DocPath, frequencies = true)
    val name2 = smartStream.stream.map(x => (x.title, x.ID))
    val len = smartStream.length
    println(s"smart stream, len $len in ${t3.elapsed()} sec, free ${Timer.totalMB()} MB")

    val df2 = doc_freq(smartStream.stream, progress = true)
    val sorted = df2.toSeq.sortWith(_._2 > _._2)
    println(s"size: ${sorted.size}, top: ${sorted.slice(0, 20)}")
  }

  def doc_freq(stream: Stream[XMLDocument], progress: Boolean = false): Map[String, Int] = {
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
