/**
  * Created by mmgreiner on 08.12.16.
  */


object IRProject2 {

  def help = {
    println("Information Retrieval Project 2 Group 11")
    println
    println("Retrieval System ")
    println("arguments: <data-directory> []")
    println
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      help
      return
    }
    val dir = args(1)


  }
}
