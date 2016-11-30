import ch.ethz.dal.tinyir.processing.{TipsterParse, Tokenizer, StopWords}
import java.io.InputStream

import ch.ethz.dal.tinyir.io.DocStream
import com.github.aztek.porterstemmer.PorterStemmer

class TipsterParseSmart(is: InputStream,
                        reduceNumbers: Boolean = true,
                        reduceStopWords: Boolean = true,
                        stemming: Boolean = true,
                        chopping: Integer = -1) extends TipsterParse(is) {

  override def ID = this.name.hashCode()

  override def tokens = TipsterParseSmart.tokenize(text = content, numbers = reduceNumbers,
    stops = reduceStopWords, stemming = stemming, chopping = chopping)

  /**
    * Compute the term frequencies.
    * @see TipsterParseSmart.tokenize about the options
    * @return a stream of tuples with the token and its frequency
    */
  def termFrequencies: Stream[(String, Int)] = {
      val tf = collection.mutable.Map[String, Int]()
      TipsterParseSmart.tokenizer(content, reduceNumbers, reduceStopWords, stemming, chopping)
        .foreach(t => {
          tf += t -> (1 + tf.getOrElse(t, 1))
        })
      tf.toStream
  }

  // remember hash codes
  TipsterParseSmart.nameHash += ID -> name

  override def title: String = {
    val n = name.substring(0, 2) match {
      case "AP" => read(doc.getElementsByTagName("HEAD")) // AP
      case "FR" => ""     // read(doc.getElementsByTagName("ITAG"))// FR need tag 7
      case "PT" => read(doc.getElementsByTagName("TTL")) // PT
      case "SJ" => read(doc.getElementsByTagName("HEADLINE")) // SJM
      case "WS" => read(doc.getElementsByTagName("HL"))
      case _ => ""
    }
    n.replaceAll("\\s+", " ")
  }
}

object TipsterParseSmart {

  val nameHash = collection.mutable.Map[Int, String]()

  // regular expressions defined statically
  val rDate = "^\\d+[/-]\\d+[/-]\\d+$".r -> "<DATE>"
  val rUSPhone = "^\\d{3}\\W\\d+{3}\\W\\d{4}$".r -> "<USPHONE>"
  val rNumber = "^[-]?\\d+([.,]\\d+)*$|^(one|two|three|four)$".r -> "<NUMBER>"
  val rTwoNum = "^\\d+[-/=]\\d+$".r -> "<NUMBER>"
  val rOrdinal = "^\\d+(th|1st|2nd|3rd)$".r -> "<ORDINAL>"
  val rLine = "--+".r -> ""                   // underlines like -----------
  val rQuote = """['\"\x60]+""".r -> ""          // also ` = x60

  /**
    * Stopwords is taken from nltk toolkit stopwords - english.
    * Convert them to a regular expression, which should be faster than contains
    */
  val StopwordsNltk = List(
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

  val rStop = (HighFreqWords ::: StopwordsNltk ::: StopWords.stopWords.toList).toSet.mkString("^(", "|", ")$").r -> ""

  private val replace = (word: String, tup: (util.matching.Regex, String)) => tup._1.replaceAllIn(word, tup._2)

  // replace lines and quotes right at the beginning
  private val trim = (word: String) => replace(replace(word, rQuote), rLine).replaceAll("""\x60+""", "")

  val stemMap = (word: String) => PorterStemmer.stem(word)

  val stopMap = (word: String) => replace(word, rStop)

  val numberMap = (word: String) => {
    var t = replace(word, rUSPhone)
    t = replace(t, rDate)
    t = replace(t, rNumber)
    t = replace(t, rTwoNum)
    replace(t, rOrdinal)
  }

  /**
    * Tokenizes the given text. Replaces numbers and dates by special tokens <NUMBER>, <TNUMBER> (= two numbers 9/11),
    * <ORDINAL> (like 1st, 2nd, ..), <DATE>
    * @param text the text to be tokenized
    * @param reduceStopWords should stopswords be replaced by <STOP>? default yes
    * @param stemming should PorterStemmer be applied? default yes
    * @return
    */
  def tokenize2(text: String,
                       reduceStopWords: Boolean = true,
                       stemming: Boolean = true): List[String] = {
    text.split("[ .,;:?!*&$-+\\s]+")
      .filter(_.length >= 3)
      .map(x => {
        // there are many misspellings around and, like andi for "and i" or "anda" for "and a".
        // however, there may be names like this also.

        var t = trim(x.toLowerCase)
        if (t.length > 0) {
          t = replace(t, rUSPhone)
          t = replace(t, rDate)
          t = replace(t, rNumber)
          t = replace(t, rTwoNum)
          t = replace(t, rOrdinal)

          if (reduceStopWords)
            t = replace(t, rStop)
          if (stemming && (t(0) != "<"))
            t = PorterStemmer.stem(t)
        }
        t
    }).filter(_.length > 0).toList
  }

  private def tokenizer (text: String, numbers: Boolean, stops: Boolean, stemming: Boolean, chopping: Int): Array[String] =
    text.split("[ .,;:?!*&$-+\\s]+")
      .filter(_.length >= 3)
      .map(x => trim(x.toLowerCase))
      .filter(_.length > 0)
      .filterNot(numbers && numberMap(_).startsWith("<"))
      .filterNot(stops && stopMap(_).length == 0)
      .map(x => if (stemming) stemMap(x) else x)
      .filter(_.length > 0)
      .map(x => if (0 < chopping && chopping < x.length) x.substring(0, chopping) else x)


  private def testTokenize(text: String, numbers: Boolean, stops: Boolean, stemming: Boolean, chopping: Int): Array[String] = {
    val a = text.split("[ .,;:?!*&$-+\\s]+").filter(_.length >= 3)
    val b = a.map(x => trim(x.toLowerCase))
    val c = b.filter(_.length > 0)
    val d = c.filterNot(numbers && numberMap(_).startsWith("<"))
    val e = d.filterNot(stops &&  stopMap(_).length == 0)
    val f = e.map(x => if (stemming) stemMap(x) else x)
    val g = f.filter(_.length > 0)
    val h = g.map(x => if (0 < chopping && chopping < x.length) x.substring(0, chopping) else x)
      .filter(_.length > 0)
    val eq = h.deep == b.deep
    h
  }

  /**
    * Tokenizes the given text.
    * @param text text string to be tokenized
    * @param numbers if true, remove all dates, numbers or phone numbers
    * @param stops if true, remove all stop words
    * @param stemming if true, perform stemming using PorterStemmer
    * @param chopping if > 0, truncate tokens to this max length
    * @return a List of tokens
    */
  def tokenize(text: String,
                numbers: Boolean = true, stops: Boolean = true, stemming: Boolean = true, chopping: Int = -1): List[String] =
    tokenizer(text, numbers, stops, stemming, chopping).toList


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