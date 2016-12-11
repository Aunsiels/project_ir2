import scala.collection.mutable.Map

object Evaluation {
    case class Stat(precision : Double,
                    recall : Double,
                    f1 : Double,
                    ap : Double){
        override def toString : String = {
            "Precision : " + precision + "\n" +
            "Recall    : " + recall    + "\n" +
            "F1        : " + f1        + "\n" +
            "(M)AP     : " + ap        
        }

        def add (that : Stat) = {
            Stat(that.precision + this.precision,
                that.recall + this.recall,
                that.f1 + this.f1,
                that.ap + this.ap)
        }
    }

    def getStat[A] (retriev : List[A], relev : List[A], beta : Double) : Stat = {
        val truePos = (relev.toSet & retriev.toSet).size.toDouble
        val precision = if (retriev.nonEmpty) truePos / retriev.size else 0.0
        //need to bound amount of relevant docs to 100 
        //because we only return top 100
        val nRelevant = math.min(relev.size, 100)
        val recall = if (relev.nonEmpty) truePos / nRelevant else 0.0
        val ap = avaragePrecision(retriev, relev)
        var f1 = 0.0
        if (truePos != 0)
            f1 = (beta * beta + 1) * precision * recall / (beta * beta * precision + recall)
        Stat(precision, recall, f1, ap)
    }

    def getStat[A] (retrievs : Map[Int, List[A]], relevs : Map[Int, List[A]], beta : Double) : Stat = {
        var sumStats = Stat(0.0, 0.0, 0.0, 0.0)
        retrievs.foreach{
          retriev =>
            sumStats = sumStats.add(getStat(retriev._2, relevs.getOrElse(retriev._1, List()), beta))
        }
      val size = retrievs.size.toDouble
        Stat(sumStats.precision / size, sumStats.recall / size, sumStats.f1 / size, sumStats.ap / size)
    }
    
    def avaragePrecision[A] (retriev : List[A], relev : List[A]) : Double = {
      var sum = 0.0
      var counter = 0.0
      var truePositives = 0.0
      val nRelevant = math.min(relev.size, 100)
      retriev.foreach{
        retr =>
          counter += 1
          if(relev.contains(retr)) truePositives += 1
          val Pk = truePositives / counter
          val relK = if(relev.contains(retr)) 1.0 else 0.0
          sum += (Pk * relK)
      }
      val avaragePrecision = (sum / nRelevant)
      avaragePrecision
    }
    
    def main(args: Array[String]): Unit = {
      val retriev1 = List("doc1", "doc2", "doc3", "doc4", "doc5", "doc6", "doc7", "doc8", "doc9", "doc10")
      val relev1 = List("doc1", "doc3", "doc6", "doc9", "doc10")
      println("AvaragePrecision: " + avaragePrecision(retriev1, relev1))
            
      val retriev2 = List("doc1", "doc2", "doc3", "doc4", "doc5", "doc6", "doc7", "doc8", "doc9", "doc10")
      val relev2 = List("doc2", "doc5", "doc7")
      println("AvaragePrecision: " + avaragePrecision(retriev2, relev2))   
      
      println(getStat(retriev1, relev1, 1.0))
      println(getStat(retriev2, relev2, 1.0))
      
      val retrievs = Map((1 -> retriev1), (2 -> retriev2))
      val relevs = Map((1 -> relev1), (2 -> relev2))
      println(getStat(retrievs, relevs, 1.0))

    }
}