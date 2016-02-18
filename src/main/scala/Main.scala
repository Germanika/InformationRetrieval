
import twit.Utils.Tokenizer
import twit.Utils.log2
import twit.Index


import scala.collection.mutable
import scala.xml.{XML, Node}


object Main {

  def main(args :Array[String]) :Unit = {
    case class Score (tweetID :String, score :Double)
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

    def idf (term :String) = {
      log2(Index.invertedIndex.size.toDouble / Index.invertedIndex(term).size.toDouble)
    }

    def retrieve (query :Query) :Seq[(String, Double)] = {
      val scores = mutable.HashMap[String, Double]()

      for (t <- query.tokens) {
        for (item <- Index.invertedIndex(t)) {
          // incrementally compute cosine similarity
          scores.put(item._1, scores.getOrElse(item._1, 0.0) + item._2 * idf(t))
          // TODO: something is going wrong in the calculations
        }
      }

      // sort by score, and return top n results
      scores.map(toResult).toSeq.sortWith(_._2 > _._2)
    }

    def toResult (item :(String, Double)) :Tuple2[String, Double] = {
      new Tuple2[String, Double] (item._1, item._2 / Index.documentLengths(item._1))
    }

    queries.foreach(q => {
      retrieve(q).take(2).foreach(println)
    })
  }
}
