/**
  * Created by mmgreiner on 08.12.16.
  */

import java.io._

object IRProject2 {

  val helpText: String =
    """
      |Information Retrieval Project 2 Group 11
      |
      |Retrieval System
      |arguments: data=<data-directory> queries=<query-file> [db=<database-file>] [relevance=<relevance-file>] [/noindex]
      |
      |   data-directory: the directory that contains the zip file with all the documents
      |   query-file: the text file with queries
      |   database-file:  a name of the index database file to use
      |   relevance-file: the csv file with relevance information
      |
    """.stripMargin

  def help(): Unit = {
    println(helpText)
  }



  /**
    * if arguments are given on the command line, take those.
    * If not, and it is one of the developers, take the info from InputFiles
    *
    * @param args
    * @return
    */
  def parseArguments(args: Array[String]): (InputFiles, Boolean) = {
    var dataDir = ""
    var queryFile = ""
    var databaseFile = "database"
    var relevanceFile = ""
    var withoutIndex = false    // repeat also without using the index

    if (args.length == 0 && InputFiles.isDeveloper)
      (InputFiles(), false)
    else {
      val patData = """da.*\s*=\s*([\w/\\\.\-]+)""".r
      val patQuery = """q.*\s*=\s*([\w/\\\.\-]+)""".r
      val patDB = """db.*\s*=\s*([\w/\\\.\-]+)""".r
      val patRel = """r.*\s*=\s*([\w/\\\.\-]+)""".r
      args.foreach {
        case patData(dir) => dataDir = dir
        case patQuery(file) => queryFile = file
        case patDB(x) => databaseFile = x
        case patRel(x) => relevanceFile = x
        case "/noindex" => withoutIndex = true
        case a => println("unknown option " + a)
      }
      (InputFiles(documents = dataDir, database = databaseFile, queries = queryFile, relevance = relevanceFile),
        withoutIndex)
    }
  }


  def main(args: Array[String]): Unit = {
    if (args.length < 1 && !InputFiles.isDeveloper) {
      help()
      return
    }
    val (input, withoutIndex) = parseArguments(args)

    // during development, don't rebuild the index
    val forceIndexRecreation = ! InputFiles.isDeveloper

    println(s"*** building index from ${input.DocPath} into database ${input.Database}, forcing $forceIndexRecreation")
    val options = TipsterOptions(
      chopping = -1,
      useSynonyms = false,
      splitLong = true
      /*, maxDocs = 20000*/)

    val index = new PersistentFreqIndex(input.DocPath, input.Database, forceIndexRecreation, options)


    println(s"*** relevance documents ${input.Relevance}")
    val relevance = RelevanceJudgementParse(input.Relevance)


    println(s"*** reading queries ${input.Queries}")
    val q: String = input.Queries
    // no clue why we need this
    val queries = QueryParse(q, options)


    println("*** models")
    val scoringOptions = ScoringModelOptions(lambda = 0.1, nDocsToBeReturned = 100, scoringMethod = "TFIDF")

    val results = collection.mutable.Map[String, Map[Int, List[String]]]()

    var models = List(
      new TermBasedModel(index, input.DocPath, options, useIndex = true),
      new LanguageModel(index, input.DocPath, options, useIndex = true))

    if (withoutIndex) {
      models = models :+ new TermBasedModel(index, input.DocPath, options, useIndex = false)
      models = models :+ new LanguageModel(index, input.DocPath, options, useIndex = false)
    }
    models
      .foreach(m => {
        println(s"model: ${m.name}")
        results += m.name -> Map[Int, List[String]]()

        val scores = m.getScores(queries.queries, scoringOptions)
        m.convertScoresToListOfDocNames(scores).foreach {
          score =>
            println("Score for query: " + score._1)
            println("proposed docs: " + score._2)
              results(m.name) += score._1 -> score._2

            /*
            println("relevant docs: " + relevance.docs.getOrElse(score._1, s"-- not found ${score._1}"))
            val stat = Evaluation.getStat(score._2, relevance.docs(score._1), 1)
            println(stat)
            */
        }

        // only save results if index is used
        if (m.useIndex)
          saveResult(scores, m)

        val overallStat = Evaluation.getStat(m.convertScoresToListOfDocNames(scores), relevance.docs, 1.0)
        println("Overall Stats: ")
        println(overallStat)

      })

    // compare results
    println("*** comparing results")
    val lm = "LanguageModel"
    val tm = "TermBasedModel"

    results(lm).keys.foreach(q => {
      val inter = results(lm)(q).toSet intersect results(tm)(q).toSet
      println(s"query $q same: ${inter.size} $inter")
    })
  }

  /**
    * generate output file with lines of the form:
    * 91 1 FR891103-0032
    * 91 2 AP880301-0271
    * ...
    * 91 100 DOE1-07-0319
    * 92 1 WSJ870129-0011
    * ...
    *
    * @param scores
    * @param model
    */
  def saveResult(scores: ScoringModel.Scores, model: ScoringModel): Unit = {
    // PrintWriter
    val fname = model match {
      case _: TermBasedModel => "ranking-t-11.run"
      case _: LanguageModel => "ranking-l-11.run"
    }
    // do not append
    val pw = new PrintWriter(new FileOutputStream(fname, false))
    scores.foreach(q => {
      q._2.zipWithIndex.foreach(d =>
        pw.println(s"${q._1} ${d._2 + 1} ${d._1._1}")
      )
    })
    pw.close()
  }

}
