/**
  * Created by mmgreiner on 26.11.16.
  */

/**
  * Class to test the performance of TipsterParseSmart
  */

import ch.ethz.dal.tinyir.io.{ParsedXMLStream, TipsterStream}
import ch.ethz.dal.tinyir.processing.XMLDocument

object PerformanceParseSmart {
  def main(args: Array[String]): Unit = {

    val bare, smart = false
    val compare = true
    val inf = InputFiles(args)
    println(s"working on ${inf.DocPath}")

    if (compare) {
      compareStreaming(new TipsterStreamSmart(inf.DocPath, numbers = false, stopWords = false, stemming = false))
      compareStreaming(new TipsterStreamSmart(inf.DocPath, numbers = true, stopWords = false, stemming = false))
      compareStreaming(new TipsterStreamSmart(inf.DocPath, numbers = true, stopWords = true, stemming = false))
      System.gc
      compareStreaming(new TipsterStreamSmart(inf.DocPath, numbers = true, stopWords = true, stemming = true))
      compareStreaming(new TipsterStreamSmart(inf.DocPath, numbers = true, stopWords = true, stemming = true, chopping = 6))
      compareStreaming(new TipsterStreamSmart(inf.DocPath, numbers = true, stopWords = true, stemming = false, chopping = 6))
    }

    // testAttributes(new TipsterStreamSmart(inf.DocPath, chopping = 6))

    if (bare) {
      var docs = new TipsterStream(inf.DocPath)
      TimeStream(docs)
      docs = null
      System.gc
    }
    if (smart) {
      var docs = new TipsterStreamSmart(inf.DocPath)
      TimeStream(docs)
      docs = null
      System.gc
    }

  }

  def TimeStream(docs: TipsterStream): Unit = {
    val t = new Timer(heapInfo = true)
    System.gc()
    val streamName = docs.getClass.getName
    println(s"$streamName: ...free ${Timer.freeMB()} MB, max ${Timer.totalMB()} MB")
    val names = docs.stream.map(x => {
      t.progress(s"${x.ID}, ${x.name}")
      (x.ID, x.name, x.tokens.length)
    }).sortBy(_._2)
    val len = names.length
    println(s"$streamName, size $len in ${t.elapsed()} sec, free ${Timer.freeMB()} MB")
    println(s"sorted is ${names.slice(0, 20)}")
  }

  /**
    * compare timing performances of various stream options
    * @param docs Stream
    */
  def compareStreaming(docs: TipsterStreamSmart, maxDocs: Int = Int.MaxValue): Unit = {
    println(s"numbers=${docs.numbers}, stopwords=${docs.stopWords}, stemming=${docs.stemming}, chopping=${docs.chopping}")
    var totTokens = 0
    var totTf = collection.mutable.Map[String, Int]()

    val t = Timer(step = 1000, heapInfo = true)
    for (doc <- docs.stream.slice(0, maxDocs)) {
      t.progress(s"${doc.name}, ${doc.title}")

      //val tokens = TipsterParseSmart.testTokenize(doc.content, docs.numbers, docs.stopWords, docs.stemming, docs.chopping)
      totTokens += doc.tokens.length
      for (tf <- doc.termFrequencies) {
        totTf += tf._1 -> (1 + totTf.getOrElse(tf._1, 1))
      }
    }
    println(s"total tokens: $totTokens, term freq: ${totTf.size}, ${totTf.head} in ${t.elapsed()} secs")
    println("*********")
  }

  def testAttributes(docs: TipsterStreamSmart): Unit = {

    var skip = ""
    val t = Timer(step = 500)
    for (doc <- docs.stream) {
      t.progress(s"${doc.name}")
      val n = doc.name.substring(0, 2)
      if (n != skip) {
        val b = doc.body
        val c = doc.codes
        val t = doc.title
        val d = doc.date
        val tokens = doc.tokens
        val tokset = tokens.toSet
        println(s"${doc.name}, $t, $d, tokens.size=${tokens.size}, tokenset=${tokset.size}")
        skip = n
      }
    }
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
