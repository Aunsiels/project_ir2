/**
  * Created by mmgreiner on 28.11.16.
  */



/**
  * special class that provides the paths to the testing files dependent on user.
  *
  * Sample usage:
  * {{{
  *   val inf = InputFiles(args)
  *   val path = inf.DocPath    // document path, either taken from args(0) or from default values
  *   val db = inf.DataBase     // database name, either taken from args(1) or from default values
  *   val testdir = inf.Docs("testdir") // path to test directory as stored in the default values
  * }}}
  *
  * @param args if args are given, take the document directory as args(0) and database filename aas args(1)
  * @param documents directory with the documents. If not given, take default of this user
  * @param database database file. If not given, take default of this user
  */
case class InputFiles(args: Array[String] = Array(), documents: String = "", database: String = "",
                      queries: String = "", relevance: String = "") {


  def Docs(key: String): String = InputFiles.defaultFiles.getOrElse(InputFiles.user, InputFiles.defaultFiles("default"))(key)

  val DocPath = if (args.length >= 1) args(0) else if (documents != "") documents else Docs("documents")
  val Database = if (args.length >= 2) args(1) else if (database != "") database else Docs("database")
  val Queries = if (args.length >= 3) args(2) else if (queries != "") queries else Docs("queries")
  val Relevance = if (args.length >= 4) args(3) else if (relevance != "") relevance else Docs("relevance")

}

object InputFiles {
  private val user = System.getProperty("user.name")

  val greinerDir = "/Users/mmgreiner/Projects/InformationRetrieval/project_ir2/"
  val merkiDir = "C:/Users/Michael/Desktop/IR Data/Project 2/"
  val defaultFiles = Map(
    "Michael" ->
      Map("database" -> "",
        "documents" -> "C:/Users/Michael/Desktop/IR Data/Project 2/documents/",
        "database" -> "C:/Users/Michael/Desktop/FreqIndexDatabase/",
        //"queries" -> "C:/Users/Michael/Desktop/IR Data/Project 2/test-questions.txt",
        "queries" -> "C:/Users/Michael/Desktop/IR Data/Project 2/questions-descriptions.txt",
        "relevance" -> "C:/Users/Michael/Desktop/IR Data/Project 2/relevance-judgements.csv"),
    "mmgreiner" ->
      Map("database" -> "/Users/mmgreiner/Projects/InformationRetrieval/project_ir2/databases/FreqIndexDatabase",
        "documents" -> "/Users/mmgreiner/Projects/InformationRetrieval/project_ir2/data/documents/",
        "queries" -> "/Users/mmgreiner/Projects/InformationRetrieval/project_ir2/data/test-questions.txt",
        "relevance" -> "/Users/mmgreiner/Projects/InformationRetrieval/project_ir2/data/relevance-judgements.csv"),
    "julien" ->
      Map("database" -> "", "documents" -> "./data/documents/"),
    "default" ->
      Map("documents" -> "./data/documents/",
        "database" -> "./database/",
        "queries" -> "./data/test-questions.txt",
        "relevance" -> "./data/relevance-judgement.csv")
  )

  val isDeveloper = defaultFiles.keys.toList contains user

}
