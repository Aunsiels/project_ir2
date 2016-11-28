import ch.ethz.dal.tinyir.processing.{TipsterParse, Tokenizer}
import java.io.InputStream

import ch.ethz.dal.tinyir.io.DocStream
import com.github.aztek.porterstemmer.PorterStemmer

class TipsterParseSmart(is: InputStream, 
                        reduceStopWords: Boolean = true,
                        stemming: Boolean = true) extends TipsterParse(is) {

  override def ID = this.name.hashCode()

  override def tokens = TipsterParseSmart.tokenize(content, reduceStopWords, stemming)

  // remember hash codes
  TipsterParseSmart.nameHash += ID -> name

}

object TipsterParseSmart {

  val nameHash = collection.mutable.Map[Int, String]()

  // regular expressions defined statically
  val rDate = ("^\\d+[/-]\\d+[/-]\\d+$".r, "<DATE>")
  val rUSPhone = ("^\\d{3}\\W\\d+{3}\\W\\d{4}$".r -> "<USPHONE>")
  val rNumber = ("^[-]?\\d+([.,]\\d+)*$|^(one|two|three|four)$".r -> "<NUMBER>")
  val rTwoNum = ("^\\d+[-/=]\\d+$".r -> "<TUMBER>")
  val rOrdinal = ("^\\d+(th|1st|2nd|3rd)$".r -> "<ORDINAL>")
  val rLine = "--+".r -> ""                   // underlines like -----------
  val rQuote = """['\"\x60]+""".r -> ""          // also ` = x60

  /**
    * Stopwords is taken from nltk toolkit stopwords - english.
    * Convert them to a regular expression, which should be faster than contains
    */
  val Stopwords = List(
    "i",
    "me",
    "my",
    "myself",
    "we",
    "our",
    "ours",
    "ourselves",
    "you",
    "your",
    "yours",
    "yourself",
    "yourselves",
    "he",
    "him",
    "his",
    "himself",
    "she",
    "her",
    "hers",
    "herself",
    "it",
    "its",
    "itself",
    "they",
    "them",
    "their",
    "theirs",
    "themselves",
    "what",
    "which",
    "who",
    "whom",
    "this",
    "that",
    "these",
    "those",
    "am",
    "is",
    "are",
    "was",
    "were",
    "be",
    "been",
    "being",
    "have",
    "has",
    "had",
    "having",
    "do",
    "does",
    "did",
    "doing",
    "a",
    "an",
    "the",
    "and",
    "but",
    "if",
    "or",
    "because",
    "as",
    "until",
    "while",
    "of",
    "at",
    "by",
    "for",
    "with",
    "about",
    "against",
    "between",
    "into",
    "through",
    "during",
    "before",
    "after",
    "above",
    "below",
    "to",
    "from",
    "up",
    "down",
    "in",
    "out",
    "on",
    "off",
    "over",
    "under",
    "again",
    "further",
    "then",
    "once",
    "here",
    "there",
    "when",
    "where",
    "why",
    "how",
    "all",
    "any",
    "both",
    "each",
    "few",
    "more",
    "most",
    "other",
    "some",
    "such",
    "no",
    "nor",
    "not",
    "only",
    "own",
    "same",
    "so",
    "than",
    "too",
    "very",
    "s",
    "t",
    "can",
    "will",
    "just",
    "don",
    "should",
    "now",
    "d",
    "ll",
    "m",
    "o",
    "re",
    "ve",
    "y",
    "ain",
    "aren",
    "couldn",
    "didn",
    "doesn",
    "hadn",
    "hasn",
    "haven",
    "isn",
    "ma",
    "mightn",
    "mustn",
    "needn",
    "shan",
    "shouldn",
    "wasn",
    "weren",
    "won",
    "wouldn"
  )

  val HighFreqWords = List(
    "would",
    "could",
    "said",
    "new",
    "on",
    "ones",
    "also"
  )

  val rStop = (HighFreqWords ::: Stopwords).mkString("^(", "|", ")$").r -> "<STOP>"

  private val replace = (word: String, tup: (util.matching.Regex, String)) => tup._1.replaceAllIn(word, tup._2)

  // replace lines and quotes right at the beginning
  private val trim = (word: String) => replace(replace(word, rQuote), rLine).replaceAll("""\x60+""", "")


  private def tokenize(text: String,
                       reduceStopWords: Boolean = true,
                       stemming: Boolean = true): List[String] = {
    text.split("[ .,;:?!*&$-+\\s]+")
      .filter(_.length >= 3)
      .map(x => {
        /*
        // there are many misspellings around and, like andi for "and i" or "anda" for "and a".
        // however, there may be names like this also.
        if ((x matches("and\\w")) && (x != "andy")) {
          val i = words.indexOf(x)
          println(s"hit and?: $x in ${words.slice(i - 5, i + 5)}")
        }
        */

        var t = trim(x.toLowerCase)
        if (t.length > 0) {
          t = replace(t, rUSPhone)
          t = replace(t, rDate)
          t = replace(t, rNumber)
          t = replace(t, rTwoNum)
          t = replace(t, rOrdinal)

          if (reduceStopWords)
            t = replace(t, rStop)
          if (stemming && t(0) != "<")
            t = PorterStemmer.stem(t)
        }
        t
    }).filter(_.length > 0).toList
  }


  def main(args: Array[String]) {

    val inf = InputFiles(args)
    val fname = inf.DocPath + "AP880212-0006"
    
    val parse = new TipsterParseSmart(DocStream.getStream(fname))
    val title = parse.title
    println(title)
    println("DocID = " + parse.ID)
    println("Date  = " + parse.date)
    println("tokens = " + parse.tokens)
    println(s"tokenset size ${parse.tokens.toSet.size}")

  }
}