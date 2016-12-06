/**
  * Created by mmgreiner on 06.12.16.
  *
  * @see https://wordnet.princeton.edu/wordnet/man/wndb.5WN.html#sect3
  *
  * @example
  * synset_offset  lex_filenum  ss_type  w_cnt  word  lex_id  [word  lex_id...]  p_cnt  [ptr...]  [frames...]  |   gloss
  * 00194969 04 n 02 variation 0 variance 0 004 @ 00407535 n 0000 + 02068277 a 0201 + 02661252 v 0202 + 02661252 v 0102 | an activity that varies from a
  */

import scala.collection.mutable
import scala.io.Source

case class Wordnet (filename: String) {

  val Synonyms: mutable.Map[String, Array[String]] = {
    val synonyms =collection.mutable.Map[String, Array[String]]()

    Source.fromFile(filename).getLines().foreach {
      case Wordnet.dataPattern(offset, w_cnt, term, words) if w_cnt.toInt > 1 =>
        val syns = Wordnet.wordPattern.findAllMatchIn(words).map(_.group(1))
        synonyms += term.replace("_", " ") -> syns.map(_.replace("_", " ")).toArray
      case Wordnet.dataPattern(offset, w_cnt, term, words) => // no synonyms
      case _ => // no synonyms
    }
    synonyms
  }
}

object Wordnet {

  private val dataPattern = """(\d+) \d+ [nvasr] (\d+) (\w+) \d+ ([\w\d ]*)\d+ [@;].*""".r
  private val wordPattern = """(\w+) \d+""".r


  def main(args: Array[String]): Unit = {
    init()
  }

  private var synonyms = collection.mutable.Map[String, Array[String]]()

  def Synonym(term: String): Array[String] = synonyms.getOrElse(term, Array(term))

  def init(directory: String = "./wordnet/") : Unit = {

    val types = List("noun", "verb", "adj")

    types.foreach(t => {
      val syns = Wordnet(directory + "data." + t).Synonyms
      println(s"$t: ${syns.size}")
      synonyms ++= syns
    })

    println(s"all synonyms: ${synonyms.size}")

    val computer = Synonym("computer").mkString("|")
    println(s"computer -> $computer")

  }
}
