import scala.collection.mutable.Map

abstract class ScoringModel (val useIndex: Boolean = true) {

  val name = this.getClass.getName


  def getScores(queries: Map[String, List[String]], scoringOptions : ScoringModelOptions) : ScoringModel.Scores
  
  def computeScoreForQuery(query : List[String], scoringOptions : ScoringModelOptions) : ScoringModel.Score
  
  def convertScoresToListOfDocNames(scores : ScoringModel.Scores) : Map[Int, List[String]] =  {
    scores.map(scores => (scores._1.toInt, scores._2.map(tuple => tuple._1).toList))
  }
  
}

object ScoringModel {
  type Score = Seq[(String, Double)]
  type Scores = Map[String, Score]
}