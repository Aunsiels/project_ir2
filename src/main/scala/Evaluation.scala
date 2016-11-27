/**
  * Created by Aun on 06/11/2016.
  */
object Evaluation {
    case class Stat(precision : Double,
                    recall : Double,
                    f1 : Double){
        override def toString : String = {
            "Precision : " + precision + "\n" +
            "Recall    : " + recall    + "\n" +
            "F1        : " + f1
        }

        def add (that : Stat) = {
            Stat(that.precision + this.precision,
                that.recall + this.recall,
                that.f1 + this.f1)
        }
    }


    // retriev = prediction
    def getStat[A] (retriev : Set[A], relev : Set[A], beta : Double) : Stat = {
        val truePos = (relev & retriev).size.toDouble
        val precision = if (retriev.size > 0) truePos / retriev.size else 0.0
        val recall = if (relev.size > 0) truePos / relev.size else 0.0
        var f1 = 0.0
        if (truePos != 0)
            f1 = (beta * beta + 1) * precision * recall / (beta * beta * precision + recall)
        Stat(precision, recall, f1)
    }

    def getStat[A] (retrievs : Iterable[Set[A]], relevs : Iterable[Set[A]], beta : Double) : Stat = {
        val sumStats = retrievs.zip(relevs).foldLeft(Stat(0.0, 0.0, 0.0))((z, s) => z.add(getStat(s._1, s._2, beta)))
        val size = relevs.size.toDouble
        Stat(sumStats.precision / size, sumStats.recall / size, sumStats.f1 / size)
    }
}