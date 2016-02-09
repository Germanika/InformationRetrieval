
import utils.Utils.Tokenizer

import scala.xml.{XML, Node}
import utils.Index

object Main {

  def main(args :Array[String]) :Unit = {
    case class Query(id :String, tokens: Array[String])

    val tokenizr = Tokenizer()
    val queryResource = getClass.getResource("/queries.txt").getFile

    val parseNode :(Node) => Query = (node) => {
      val num :String = (node \ "num").text.substring(9, 14)
      val title :String = (node \ "title").text.toLowerCase()
      Query(num, tokenizr(title))
    }

    val queries :List[Query] =
      (XML.loadFile(queryResource) \ "top")
        .map(parseNode)
        .toList

    val twitterIndex = new Index()

    def getResult (q :Query) :Seq[String] = {
      q.tokens.map( (token :String) => {
        twitterIndex.invertedIndex.get(token)
      }).flatten
    }

//    twitterIndex.documentFrequency.toSeq.sortWith(sortDf).take(50).foreach(println)
  }
}
