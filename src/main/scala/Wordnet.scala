/**
  * Created by mmgreiner on 06.12.16.
  *
  * @see https://wordnet.princeton.edu/wordnet/man/wndb.5WN.html#sect3
  *
  * @example
  * synset_offset  lex_filenum  ss_type  w_cnt  word  lex_id  [word  lex_id...]  p_cnt  [ptr...]  [frames...]  |   gloss
  * 00194969 04 n 02 variation 0 variance 0 004 @ 00407535 n 0000 + 02068277 a 0201 + 02661252 v 0202 + 02661252 v 0102 | an activity that varies from a
  */

import com.github.aztek.porterstemmer.PorterStemmer

import scala.collection.mutable
import scala.io.Source

/**
  * class that reads the wordnet files and returns a list of synomys
  * @param directory directory of the wordnet files
  * @param types list of strings which are "noun", "verb", "adj"
  */
case class Wordnet (directory: String = "./wordnet/", types: List[String] = List("noun", "verb", "adj")) {

  private var synonyms =collection.mutable.Map[String, Array[String]]()
  private var synonymsStem = collection.mutable.Map[String, Array[String]]()


  types.foreach(t => t match {
    case "noun" | "verb" | "adj" => addSynonyms(directory + "data." + t)
    case _ => println(s" $t not known")
  })


  private def addSynonyms(filename: String) = {
    val synonyms = collection.mutable.Map[String, Array[String]]()
    Source.fromFile(filename).getLines().foreach {
      case Wordnet.dataPattern(offset, w_cnt, term, words) if w_cnt.toInt > 1 =>
        val syns = Wordnet.wordPattern.findAllMatchIn(words).map(_.group(1))
        val term2 = term.replace("_", " ")
        val synClean = syns.map(_.replace("_", " "))
        synonyms += term2 -> synClean.toArray
        synonymsStem += PorterStemmer.stem(term2) -> synClean.map(PorterStemmer.stem(_)).toArray
      case Wordnet.dataPattern(offset, w_cnt, term, words) => // no synonyms
      case _ => // no synonyms
    }
  }

  def get (word: String, stemming: Boolean = false): Array[String] =
    if (! stemming) synonyms.getOrElse(word, Array[String]())
    else synonymsStem.getOrElse(word, Array[String]())

  def expandBySynoyms(terms: String, stemming: Boolean = false): String = {

    val termList = TipsterParseSmart.tokenize(terms, numbers = false, stopWords = false, stemming = stemming)
    var newterms = terms
    termList.foreach(t => newterms += get(t, stemming))
    termList.mkString(" ")
  }
}

object Wordnet {

  private val dataPattern = """(\d+) \d+ [nvasr] (\d+) (\w+) \d+ ([\w\d ]*)\d+ [@;].*""".r
  private val wordPattern = """(\w+) \d+""".r

  def main(args: Array[String]): Unit = {
    val wordnet = Wordnet()

    val computer = wordnet.get("computer").mkString("|")
    println(s"computer -> $computer")

    var term = "examination"
    var synonym = wordnet.get(term).mkString("|")
    println(s"$term -> $synonym")

    term = "president"
    synonym = wordnet.get(term).mkString("|")
    println(s"$term -> $synonym")

    term = "gas"
    synonym = wordnet.get(term).mkString("|")
    println(s"$term -> $synonym")

  }


}
