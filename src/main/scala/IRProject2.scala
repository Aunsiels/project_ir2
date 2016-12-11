/**
  * Created by mmgreiner on 08.12.16.
  */

import java.io._

object IRProject2 {

  val helpText =
    """
      |Information Retrieval Project 2 Group 11
      |
      |Retrieval System
      |arguments: data=<data-directory> queries=<query-file> [db=<database-file>] [relevance=<relevance-file>]]
      |
    """.stripMargin

  def help() = {
    println(helpText)
  }

  var dataDir = ""
  var queryFile = ""
  var databaseFile = ""
  var relevanceFile = ""

  def parseArguments(args: Array[String]): InputFiles = {
    val patData = """da.*\s*=\s*([\w/\\\.\-]+)""".r
    val patQuery = """q.*\s*=\s*([\w/\\\.\-]+)""".r
    val patDB = """db.*\s*=\s*([\w/\\\.\-]+)""".r
    val patRel = """r.*\s*=\s*([\w/\\\.\-]+)""".r
    args.foreach {
      case patData(dir) => dataDir = dir
      case patQuery(file) => queryFile = file
      case patDB(x) => databaseFile = x
      case patRel(x) => relevanceFile = x
      case a => println("unknown option " + a)
    }
    InputFiles(documents = dataDir, database = databaseFile, queries = queryFile, relevance = relevanceFile)
  }


  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      help
      return
    }
    val input = parseArguments(args)

    println("*** building index")
    val options = TipsterOptions(chopping = -1, useSynonyms = true /*, maxDocs = 20000*/)
    val forceIndexRecreation = false

    val index = new PersistentFreqIndex(input.DocPath, input.Database, forceIndexRecreation, options)


    println("*** relevance documents")
    val relevance = RelevanceJudgementParse(input.Relevance)


    println("*** reading queries")
    val q: String = input.Queries
    // no clue why we need this
    val queries = QueryParse(q, options)


    println("*** models")
    val scoringOptions = ScoringModelOptions(scoringMethod = "COSINEDISTANCE")

    List(
      new TermBasedModel(index, input.DocPath, options, useIndex = false),
      new TermBasedModel(index, input.DocPath, options, useIndex = true),
      new LanguageModel(index, input.DocPath, options, useIndex = false),
      new LanguageModel(index, input.DocPath, options, useIndex = true)
    )
      .foreach(m => {
        println(s"model: ${m.name}")
        val scores = m.getScores(queries.queries, scoringOptions)
        m.convertScoresToListOfDocNames(scores).foreach {
          score =>
            println("Score for query: " + score._1)
            println("relevant docs: " + relevance.docs(score._1))
            println("proposed docs: " + score._2)
            val stat = Evaluation.getStat(score._2, relevance.docs(score._1), 1)
            println(stat)
        }

        // only save results if index is used
        if (m.useIndex)
          saveResult(scores, m)

        val overallStat = Evaluation.getStat(m.convertScoresToListOfDocNames(scores), relevance.docs, 1.0)
        println("Overall Stats: ")
        println(overallStat)

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
  def saveResult(scores: ScoringModel.Scores, model: ScoringModel) = {
    // PrintWriter
    val fname = model match {
      case _: TermBasedModel => "ranking-t-11.run"
      case _: LanguageModel => "ranking-l-11.run"
    }
    // do not append
    val pw = new PrintWriter(new FileOutputStream(fname, false))
    scores.foreach(q => {
      q._2.zipWithIndex.foreach(d =>
        pw.println(s"${q._1} ${d._2} ${d._1}")
      )
    })
    pw.close()
  }
}
