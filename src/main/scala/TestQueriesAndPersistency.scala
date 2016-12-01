/**
  * Created by mmgreiner on 01.12.16.
  */
object TestQueriesAndPersistency {
  def main(args: Array[String]): Unit = {
    val inf = InputFiles(args)

    val options = TipsterOptions(maxDocs = 50000)
    val index = new PersistentFreqIndex(inf.DocPath, inf.Database, true)
    val queries = QueryParse(inf.Queries, options)
    println(s"survived")
  }
}
