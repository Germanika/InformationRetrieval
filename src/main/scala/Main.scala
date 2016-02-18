
import twit.Utils.Tokenizer
import twit.Index


import scala.xml.{XML, Node}

object Main {

  def main(args :Array[String]) :Unit = {
    case class Query(id :String, tokens: Array[String])

    val tokenizr = Tokenizer()
    val queryResource = getClass.getResource("/queries.txt").getFile
    println(queryResource)

    val parseNode :(Node) => Query = (node) => {
      val num :String = (node \ "num").text.substring(9, 14)
      val title :String = (node \ "title").text.toLowerCase()
      Query(num, tokenizr(title))
    }

    val queries :List[Query] =
      (XML.loadFile(queryResource) \ "top")
        .map(parseNode)
        .toList

    println(Index.invertedIndex.size)
    println(Index.documentLengths.size)
    Index.documentLengths.foreach(println)
  }
}
