import ch.ethz.dal.tinyir.processing.TipsterParse
import java.io.InputStream
import ch.ethz.dal.tinyir.io.DocStream
import com.github.aztek.porterstemmer.PorterStemmer

class TipsterParseSmart(is: InputStream, 
                        reduceStopWords: Boolean = false, 
                        stemming: Boolean = false) extends TipsterParse(is) {

  val raw_tokens = super.tokens
  
  override def ID = this.name.hashCode()
  
  private val replace = (word: String, tup: Tuple2[util.matching.Regex, String]) => tup._1.replaceAllIn(word, tup._2)

  private def tokenize(words: List[String]): List[String] = {
    words.flatMap(x => {
      var t = x
      t = replace(t, TipsterParseSmart.rLine)
      t = replace(t, TipsterParseSmart.rUSPhone)
      t = replace(t, TipsterParseSmart.rDate)
      t = replace(t, TipsterParseSmart.rNumber)
      t = replace(t, TipsterParseSmart.rTwoNum)
      t = replace(t, TipsterParseSmart.rOrdinal)
      t = replace(t, TipsterParseSmart.rPunct)

      if (reduceStopWords)
        t = replace(t, TipsterParseSmart.rStop)
      if (stemming)
        t = PorterStemmer.stem(t)

      if (reduceStopWords)
        t = replace(t, TipsterParseSmart.rFreqWords)

      t.replaceAll("\\s", " ")
        .split(" ")
    })
      .filterNot(_=="")
  }

  private var _smartTokens: List[String] = _

  override def tokens = {
    if (_smartTokens == null)
      _smartTokens = tokenize(raw_tokens)
    _smartTokens
  }

  val vocabulary = tokens.toSet



  def test(stream: TipsterStreamSmart, howMany: Integer = 5000): Unit = {
    val docs = stream.stream.slice(0, howMany)
    val rawset = raw_tokens.toSet
    println(s"nof raw_tokens ${raw_tokens.size}, nof smart token ${tokens.size}, ${vocabulary.size}")
  }  
}

object TipsterParseSmart {
  // regular expressions defined statically
  val rDate = ("^\\d+[/-]\\d+[/-]\\d+$".r, "<DATE>")
  val rUSPhone = ("^\\d{3}\\W\\d+{3}\\W\\d{4}$".r -> "<USPHONE>")
  val rNumber = ("^[-]?\\d+([.,]\\d+)*$".r -> "<NUMBER>")
  val rTwoNum = ("^\\d+[-/=]\\d+$".r -> "<TUMBER>")
  val rOrdinal = ("^\\d+(th|1st|2nd|3rd)$".r -> "<ORDINAL>")
  val rPunct = ("[,;.:]$".r -> " <PUNCT>")      // if it is like "end.", should return "end <PUNCT>"
  val rLine = ("--+".r -> "")                   // underlines like -----------

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
  val rStop = Stopwords.mkString("^(", "|", ")$").r -> "<STOP>"

  val HighFreqWords = List(
    //"share",     // -> 32117,
    //"bank",      // -> 30300,
    //"million",   // -> 45624,
    "would",     // -> 35039,
    //"percent",   //  -> 53790,
    "year",      //  -> 40026,
    //"market",    // -> 35931,
    "said",      // -> 155872,
    "new"        // -> 33655
  )
  val rFreqWords = HighFreqWords.mkString("^(", "|", ")$").r -> "<HIFREQ>"

  def main(args: Array[String]) {

    //val dir = "/Users/mmgreiner/Projects/InformationRetrieval/data/score2/train-orig/"
    //val fname = dir + "100009newsML.xml"

    val dir = "C:/Users/Michael/Desktop/IR Data/Project 2/"
    val fname = dir + "AP880212-0006"
    
    val parse = new TipsterParseSmart(DocStream.getStream(fname), reduceStopWords = true, stemming = true)
    val title = parse.title
    println(title)
    println("DocID = " + parse.ID)
    println("Date  = " + parse.date)
    println("Codes = " + parse.codes.mkString(" "))
    println("raw_tokens = " + parse.raw_tokens)
    println(s"raw  set size ${parse.raw_tokens.size}")
    println("tokens = " + parse.tokens)
    println(s"tokenset size ${parse.tokens.toSet.size}")

  }
}