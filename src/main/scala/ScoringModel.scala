import scala.collection.mutable.Map

abstract class ScoringModel {
      
  def getScores(queries: Map[String, List[String]], scoringOptions : ScoringModelOptions) : Map[String, Seq[(String, Double)]]
  
  def computeScoreForQuery(query : List[String], scoringOptions : ScoringModelOptions) : Seq[(String, Double)]
  
  def convertScoresToListOfDocNames(scores : Map[String, Seq[(String, Double)]]) : Map[Int, List[String]] =  {
    scores.map(scores => (scores._1.toInt, scores._2.map(tuple => tuple._1).toList))
  }
  
}