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
case class InputFiles(args: Array[String] = Array(), documents: String = "", database: String = "") {

  private val user = System.getProperty("user.name")

  def Docs(key: String): String = InputFiles.defaultFiles.getOrElse(user, InputFiles.defaultFiles("default"))(key)

  val DocPath = if (args.length >= 1) args(0) else Docs("documents")
  val Database = if (args.length >= 2) args(1) else Docs("database")
  val Queries = if (args.length >= 3) args(1) else Docs("queries")
  val Relevance = if (args.length >= 4) args(1) else Docs("relevance")

}

object InputFiles {
  val defaultFiles = Map(
    "Michael" ->
      Map("database" -> "",
        "documents" -> "C:/Users/Michael/Desktop/IR Data/Project 2/documents/",
        "database" -> "C:/Users/Michael/Desktop/FreqIndexDatabase/",
        "queries" -> "C:/Users/Michael/Desktop/IR Data/Project 2/questions-descriptions.txt",
        "relevance" -> "C:/Users/Michael/Desktop/IR Data/Project 2/relevance-judgements.csv"),
    "mmgreiner" ->
      Map("database" -> "./databases/FreqIndexDatabase_100000",
        "documents" -> "./data/documents/",
        "queries" -> "./data/questions-descriptions.txt",
        "relevance" -> "./data/relevance-judgements.csv"),
    "julien" ->
      Map("database" -> "", "documents" -> "./data/documents/"),
    "default" ->
      Map("documents" -> "./data/documents/", "database" -> "./database/")
  )

}
