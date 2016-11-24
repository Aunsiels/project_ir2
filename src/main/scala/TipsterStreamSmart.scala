
import ch.ethz.dal.tinyir.io.TipsterStream
import java.io.{FileOutputStream, PrintWriter}
import scala.collection.JavaConversions._

class TipsterStreamSmart(path: String, 
                         ext: String = "",
                         stopWords: Boolean = false, 
                         stemming: Boolean = false, 
                         maxDocs: Int = Int.MaxValue ) extends TipsterStream(path, ext) {
  
  override def stream : Stream[TipsterParseSmart] =
    unparsed.stream.slice(0, maxDocs).map(is => new TipsterParseSmart(is, stopWords, stemming))
}

object TipsterStreamSmart {

  def testVocabulary(dir: String, stopWords: Boolean, stemming: Boolean): Unit = {
    val t = new Timer(heapInfo = true)
    println(s"heap ${Timer.freeMB()}, stopwords=$stopWords, stemming=$stemming")
    val stream = new TipsterStreamSmart(dir, stopWords = stopWords, stemming = stemming)
    var vocabularyAll = collection.mutable.Set[String]()
    var totalTokens = 0
    var rawVocabulary = collection.mutable.Set[String]()

    val vocabulary = stream.stream.foreach(x => {
      vocabularyAll = vocabularyAll union x.vocabulary
      totalTokens += x.tokens.size
      t.progress(s"${x.title}, ${vocabularyAll.size}")
    })
    println(s"nof distinct words ${vocabularyAll.size}, totalTokens ${totalTokens}")
  }


  def main(args: Array[String]): Unit = {
    //val dir = "/Users/mmgreiner/Projects/InformationRetrieval/data/score2/train/"
    val dir = "C:/Users/Michael/Desktop/IR Data/Project 2/documents/"
    
    /*testVocabulary(dir, false, false)
    System.gc
    testVocabulary(dir, true, false)
    System.gc*/
    testVocabulary(dir, true, true)
  }
}