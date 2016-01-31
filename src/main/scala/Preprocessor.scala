import java.io.InputStream

import scala.io.Source

object Preprocessor {

  def removeStopWords () = {
    readFromResource("StopWords")
        .foreach(println)
  }

  def readFromResource (name: String): Iterator[String] = {
    val jStream: InputStream = getClass.getResourceAsStream(name)
    Source.fromInputStream( jStream ).getLines()
  }


  def main (args : Array[String]): Unit = {
    removeStopWords
  }
}
