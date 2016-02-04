
import utils.Utils.Tokenizer

import scala.collection.mutable
import scala.xml.{XML, Node}

object Main {

  def main(args: Array[String]): Unit = {
    case class Query(id: String, tokens: Array[String])

    val tokenizr = Tokenizer()
    val queryResource = getClass.getResource("/queries.txt").getFile

    val parseNode: (Node) => Query = (node) => {
      val num :String = (node \ "num").text.substring(9, 14)
      val title :String = (node \ "title").text.toLowerCase()
      Query(num, tokenizr(title))
    }

    val queries : List[Query] =
      (XML.loadFile(queryResource) \ "top")
        .map(parseNode)
        .toList

    val twitterIndex = new Index()

    println(twitterIndex.tweets.size)
    println(twitterIndex.invertedIndex.size)

//    def sortingFunction(m1 :(String, Int), m2 :(String, Int)): Boolean = {
//      return m1._2 > m2._2
//    }
//
//    twitterIndex.documentFrequency.toSeq.sortWith(sortingFunction).take(10).foreach(println)

  }
}
