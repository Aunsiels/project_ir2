import scala.io.Source
import scala.collection.mutable.Map

class QueryParse(fname: String) {
  
  private var topics = Map[String, List[String]]()
  
  def queries : Map[String, List[String]] = this.topics 
     
  private var number : String = ""
  private var topic : String = ""    
  for(line <- Source.fromFile(fname).getLines()) {
     if(line.contains("<num>")) {
        number = line.substring(line.indexOf(':')+1).replace(" ", "")
      }
      if(line.contains("<title>")) {
        topic = line.substring(line.indexOf(':') + 1)
        while(topic.startsWith(" ")) {
          topic = topic.substring(1)
        }
        var query = TipsterParseSmart.tokenize(topic, true, true, true, 6)
        topics += number -> query
      }
    }
  //println(topics)
  //println(topics.size)
}

object QueryParse {
  def main(args: Array[String]) {
    
    val dirname = "C:/Users/Michael/Desktop/IR Data/Project 2"
    val fname = dirname + "/questions-descriptions.txt" 
    var queryParse = new QueryParse(fname)  
    
  }
}