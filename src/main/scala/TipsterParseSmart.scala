/**
  * @author mmgreiner
  * @version
  */

import ch.ethz.dal.tinyir.processing.{StopWords, TipsterParse}
import java.io.{InputStream, FileInputStream}

import ch.ethz.dal.tinyir.io.DocStream
import com.github.aztek.porterstemmer.PorterStemmer

import scala.util.{Try, Failure, Success}


class TipsterParseSmart(is: InputStream,
                        reduceNumbers: Boolean = true,
                        reduceStopWords: Boolean = true,
                        stemming: Boolean = true,
                        chopping: Integer = -1,
                        ngramSize: Integer = 0,
                        splitLong: Boolean) extends TipsterParse(is) {

  def this(is: InputStream, options: TipsterOptions) =
    this(is, options.numbers, options.stopWords, options.stemming, options.chopping, options.ngramSize, options.splitLong)

  def this(fileName: String, options: TipsterOptions) =
    this(new FileInputStream(fileName), options)


  override def ID = this.name.hashCode()

  override def tokens = TipsterParseSmart.tokenize(content, reduceNumbers, reduceStopWords, stemming, chopping,
    ngramSize, splitLong)

  /**
    * Compute the term frequencies.
    *
    * @see TipsterParseSmart.tokenize about the options
    * @return a stream of tuples with the token and its frequency
    */
  def termFrequencies: Stream[(String, Int)] = {
    val tf = collection.mutable.Map[String, Int]()
    TipsterParseSmart.tokenizer(content, reduceNumbers, reduceStopWords, stemming, chopping, ngramSize, splitLong)
      .foreach(t => {
        tf += t -> (1 + tf.getOrElse(t, 0))
      })
     TipsterParseSmart.tokenizer(title, reduceNumbers, reduceStopWords, stemming, chopping, ngramSize, splitLong)
      .foreach(t => {
        tf += t -> (5 + tf.getOrElse(t, 0))
      })  
    tf.toStream
  }

  def termFrequency(term: String): Int = {
    val tokens = TipsterParseSmart.tokenizer(content, reduceNumbers, reduceStopWords, stemming, chopping, ngramSize, splitLong)
    tokens.count(_.equals(term))
  }

  def getDocumentLength: Int = {
    tokens.length
  }

  // remember hash codes
  TipsterParseSmart.nameHash += ID -> name

  private val bodyStart = if (body.length > TipsterParseSmart.TitleCutoff)
    body.substring(0, TipsterParseSmart.TitleCutoff) else body

  override def title: String = {
    // AP|DOE|FR|PT|SJM|WSJ
    name.substring(0, 2) match {
      case "AP" => read(doc.getElementsByTagName("HEAD")) // AP
      case "DO" => bodyStart
      case "FR" => bodyStart // read(doc.getElementsByTagName("ITAG"))// FR need tag 7
      case "PT" => read(doc.getElementsByTagName("TTL")) // PT
      case "SJ" => read(doc.getElementsByTagName("HEADLINE")) // SJM
      case "WS" => read(doc.getElementsByTagName("HL"))
      case "ZF" => read(doc.getElementsByTagName("TITLE"))
      case _ => bodyStart
    }
  }

  def titleTokens: Array[String] = TipsterParseSmart.tokenizer(title, reduceNumbers, reduceStopWords,
    stemming, chopping, ngramSize, splitLong = splitLong)
}


object TipsterParseSmart {

  val TitleCutoff = 100

  val nameHash = collection.mutable.Map[Int, String]()

  // regular expressions defined statically
  val Split = """[ .,;:?!*&$\-+_=/\s\(\)\[\]\{\}'\"\x60\n\r]+"""
  val rDate = "^\\d+[/-]\\d+[/-]\\d+$".r -> "<DATE>"
  val rUSPhone = "^\\d{3}\\W\\d+{3}\\W\\d{4}$".r -> "<USPHONE>"
  val rNumber = "^[-]?\\d+([.,]\\d+)*$|^(one|two|three|four)$".r -> "<NUMBER>"
  val rTwoNum = "^\\d+[-/=]\\d+$".r -> "<NUMBER>"
  val rOrdinal = "^\\d+(th|1st|2nd|3rd)$".r -> "<ORDINAL>"
  val rLine = "--+".r -> ""     // underlines like -----------
  val rQuote = """['\"\x60]+""".r -> "" // also ` = x60

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

  // replace lines and quotes right at the beginning. Back-quotes are somewhat tricky, \x60
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

  val MidCap = "(.+)([A-Z].*)".r
  val patIonal = "(.+ionally|.+ional|.+ions?)(.{3,})".r
  val patIng = "(.+ingly|.+ing|.+iously|.+ious|.+ment)(.{3,})".r
  val patStop = "(their|they|there|these|the|those|without|with|was|and|have)(.+)".r
  val patEd = "(.{4,}ed)(.{4,})".r
  val patAnd = "([Aa]nd)([^aeiory].{3,})".r
  val patThe = "([Tt]he)([dhjpvxz].{3,})".r
  val patXXX = "(became|\\d{3,})(.{4,})".r
  val patXXY = "(accounted|composed)(.{2,})".r
  val patYYY = "(.{4,})(that|which|\\d{3,})".r

  var andCount = 0
  var theCount = 0
  var xxxCount = 0
  var splitCount = 0
  /**
    * try to split long words which in most cases have missing blanks and are misspelled
    * @param word
    * @return
    */
  private def splitLongWords(word: String): Array[String] = {
    if (word.length <= 15)
      word match {
        case MidCap(front, back) => splitCount += 1; Array(front, back)
        case patAnd(and, back) => andCount += 1; Array(and, back)
        case patThe(the, back) => theCount += 1; Array(the, back)
        case patXXX(xxx, back) => xxxCount += 1; Array(xxx, back)
        case patXXY(xxx, back) => xxxCount += 1; Array(xxx, back)
        case patYYY(front, back) => xxxCount += 1; Array(front, back)
        case _ => Array(word)
      }
    else
      word match {
        case patIonal(front, back) => splitCount += 1; Array(front, back)
        case patIng(front, back)  => splitCount += 1; Array(front, back)
        case patStop(front, back) => splitCount += 1; Array(front, back)
        case patEd(front, back)   => splitCount += 1; Array(front, back)
        case _ => Array(word)
      }
  }

  private val StrangeWords = Set("24trip",
    "286architectur",
    "305elig",
    "33938pesticid",
    "3877feder",
    "41446west",
    "46north",
    "501et",
    "59supervis",
    "6563permit",
    "73005norman",
    "80386or",
    "88c08",
    "95i",
    """\52\\51\""",
    "aboutgroundwat",
    "accountedfor",
    "actionvideo",
    "adequateadvertis",
    "aeronautico",
    "againstsdi",
    "aintellig",
    "aliphaticsatur",
    "almetermethod",
    "alwaysthat",
    "amrevis",
    "andnov",
    "anonsystem",
    "anysubsequ",
    "apportionthat",
    "are48",
    "arizonasen",
    "asclust",
    "assetscould",
    "atequit",
    "auctionsin",
    "averagingperiod",
    "bailoutcould",
    "basedcriteria",
    "becamepresid",
    "beforeestablish",
    "benderandm",
    "betweendist",
    "biolipid",
    "boardprivaci",
    "bothcalifornia",
    "brianpolcari",
    "buffalony14127",
    "butchief",
    "byibc",
    "cai7",
    "candidatesregist",
    "cargoboat",
    "caulanthu",
    "certainfunct",
    "characterread",
    "chinesetelephon",
    "citicorpstak",
    "cleviceswith",
    "coderun",
    "comilla",
    "companiesor",
    "composedof",
    "conferencereport",
    "constitutesfin",
    "contractsobtain",
    "cornerthat",
    "counselind",
    "cpito",
    "crowdedrefuge",
    "customermonei",
    "danko",
    "dea4stn",
    "defaultselect",
    "demandpro",
    "device=vegabio",
    "digeorg",
    "disclosingor",
    "distributorform",
    "domesticationin",
    "drasticallymost",
    "duplicatetest",
    "easementwhen",
    "effectiveact",
    "electroniandm",
    "employerand",
    "enhanceperform",
    "equipmentdoor",
    "etector",
    "evidencewhatsoev",
    "existingact",
    "expresslyth",
    "factorwhich",
    "favoritecandid",
    "ferntre",
    "filterclog",
    "firstpay",
    "flowingtext",
    "forcehad",
    "formalsever",
    "forvoac",
    "freelyconvert",
    "fromnowher",
    "fundreson",
    "garnerenough",
    "geneticpattern",
    "giveiraq",
    "gordonl",
    "greatestreturn",
    "guardrulesout",
    "hakefisheri",
    "hasauthor",
    "hazardsevalu",
    "helpdoctor",
    "hidronephrosi",
    "hisreced",
    "hopingw")

  private def tokenizer(text: String, numbers: Boolean, stops: Boolean,
                        stemming: Boolean, chopping: Int, ngramSize: Int = 0, splitLong: Boolean): Array[String] = {
    val toks0 = text.split(Split)
      .filter(_.length >= 3)
      .flatMap(x => if (splitLong) splitLongWords(x) else Array(x))

    val toks = toks0.map(x => trim(x.toLowerCase))
      .filter(_.length > 0)
      .filterNot(numbers && numberMap(_).startsWith("<"))
      .filterNot(stops && stopMap(_).length == 0)
      .map(x => if (stemming) stemMap(x) else x)
      .filter(_.length > 0)
      .map(x => if (0 < chopping && chopping < x.length) x.substring(0, chopping) else x)
      .filter(_.length > 0)

    if (ngramSize > 0)
      toks.flatMap(ngrams(_, ngramSize))
    else
      toks
  }


  def ngrams(text: String, n: Int): Array[String] = {
    if (n < 1 || text.length < n) Array[String](text)
    else {
      var ngrams = Array[String]()
      for (ngram <- text.sliding(n)) {
        ngrams = ngrams :+ ngram
      }
      ngrams
    }
  }

  private def testTokenize(text: String, numbers: Boolean, stops: Boolean, stemming: Boolean, chopping: Int): Array[String] = {
    val a = text.split(Split).filter(_.length >= 3)
    val b = a.map(x => trim(x.toLowerCase))
    val c = b.filter(_.length > 0)
    val d = c.filterNot(numbers && numberMap(_).startsWith("<"))
    val e = d.filterNot(stops && stopMap(_).length == 0)
    val f = e.map(x => if (stemming) stemMap(x) else x)
    val g = f.filter(_.length > 0)
    val h = g.map(x => if (0 < chopping && chopping < x.length) x.substring(0, chopping) else x)
      .filter(_.length > 0)
    val eq = h.deep == b.deep
    h
  }

  /**
    * Tokenizes the given text.
    *
    * @param text      text string to be tokenized
    * @param numbers   if true, remove all dates, numbers or phone numbers
    * @param stopWords if true, remove all stop words
    * @param stemming  if true, perform stemming using PorterStemmer
    * @param chopping  if > 0, truncate tokens to this max length
    * @return a List of tokens
    */
  def tokenize(text: String,
               numbers: Boolean = true, stopWords: Boolean = true, stemming: Boolean = true,
               chopping: Int = -1, ngramSize: Int = 0, splitLong: Boolean = true): List[String] =
    tokenizer(text, numbers, stopWords, stemming, chopping, ngramSize, splitLong).toList

  def tokenize(text: String, options: TipsterOptions): List[String] =
    tokenizer(text, options.numbers, options.stopWords, options.stemming,
      options.chopping, options.ngramSize, options.splitLong).toList


  val logging = true

  def main(args: Array[String]) {

    val inf = InputFiles(args)
    val fname = inf.DocPath + "AP880212-0006"

    val options = TipsterOptions(maxDocs = 10)
    val parse = new TipsterParseSmart(DocStream.getStream(fname), options)
    val title = parse.title
    println(title)
    println("DocID = " + parse.ID)
    println("Date  = " + parse.date)
    println("tokens = " + parse.tokens)
    println(s"tokenset size ${parse.tokens.toSet.size}")
    parse.termFrequencies.foreach {
      ngram =>
        println(ngram)
    }
  }
}