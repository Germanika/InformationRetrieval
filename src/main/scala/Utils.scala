package utils
import java.io.InputStream

import scala.io.Source

object Utils {
  def readFromResource (name: String): Iterator[String] = {
    val jStream: InputStream = getClass.getResourceAsStream(name)
    Source.fromInputStream(jStream).getLines()
  }

  // TODO: remove links
  case class Tokenizer(regex: String = "[^a-z]+") {
    val stopWords = readFromResource("/StopWords").toSet

    def apply(str: String): Array[String] = {
      str.toLowerCase.split(regex).filter(!stopWords.contains(_))
    }
  }
}
