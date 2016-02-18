
import twit.Utils.Tokenizer
import twit.Utils.log2
import twit.Index


import scala.collection.mutable
import scala.xml.{XML, Node}


object Main {

  def main(args :Array[String]) :Unit = {
    case class Score (tweetID :String, score :Double)
    case class Query(id :String, tokens: Array[String])
    var documentLengths = Map[String, Double]() // tweetID -> document length


    val tokenizr = Tokenizer()
    val queryResource = getClass.getResource("/queries.txt").getFile
    println(queryResource)

    // gets query info from an xml node
    val parseNode :(Node) => Query = (node) => {
      val num :String = (node \ "num").text.substring(9, 14)
      val title :String = (node \ "title").text.toLowerCase()
      Query(num, tokenizr(title))
    }

    val queries :List[Query] =
      (XML.loadFile(queryResource) \ "top")
        .map(parseNode)
        .toList

    def idf (total :Double, relevant :Double) = {
      log2(total / relevant)
    }

    def tokenIdf (term :String) = {
      idf(Index.invertedIndex.size.toDouble, Index.invertedIndex(term).size.toDouble)
    }

    def getLength (values :List[Double]) :Double = {
      math.sqrt(values.foldLeft(0.0)((accum :Double, x :Double) =>
        accum + math.pow(x, 2)))
    }

    // list of tweets and their tf-idf values from a token
    def tweetsByToken (token :String) :List[Tuple2[Double, String]] = {
      val termIdf = tokenIdf(token)
      Index.invertedIndex(token).map(item => {
        Tuple2(item._2 * termIdf, item._1)
      }).toList
    }

    def getDocumentLengths: Map[String, Double] = {
      documentLengths = Map[String, Double]()
      Index.tweets.keys.foreach((tid: String) => {
        documentLengths += (tid -> getDocumentLength(tid))
      })
      documentLengths
    }

    def getDocumentLength (tid: String) :Double = {
      math.sqrt(tokenizr(Index.tweets.getOrElse(tid, ""))
        .map( token => {
          math.pow(getTokenWeight(token, tid) , 2)
        })
        .sum
      )
    }

    def getTokenWeight (token :String, tid :String) :Double = {
      val idf :Double = tokenIdf(token)
      val tf :Int = Index.invertedIndex.getOrElse(token, Map(tid -> 0))
        .getOrElse(tid, 0)
      idf * tf.toDouble
    }

    def getQueryLength (query :Query) :Double = {
      math.sqrt(
        query.tokens.map( token => {
          math.pow(tokenIdf(token), 2)
        })
        .sum
      )
    }

    def retrieve (query :Query) :Seq[(String, Double)] = {
      val scores = mutable.HashMap[String, Double]()
      val tokens = query.tokens.filter( (t :String) => Index.invertedIndex.contains(t))
      val tokenFrequencyInQuery = tokens.foldLeft(mutable.Map[String, Int]())( (frequencies :mutable.Map[String, Int], token :String) => {
        frequencies += token -> (frequencies.getOrElse(token, 0) + 1)
      })

      val maxTokenFrequencyInQuery :Int = tokenFrequencyInQuery.foldLeft(0)(
        (max :Int, item :Tuple2[String, Int]) => {
          if (max > item._2) max
          else item._2
        }
      )

      val uniqueTokens = tokens.toSet

      val queryTokenIdfs :mutable.Map[String, Double] =
        uniqueTokens.foldLeft(mutable.Map[String, Double]())(
          (idfs :mutable.Map[String, Double], token :String) => {
            idfs += token -> tokenIdf(token)
          }
        )

      val queryTfIdfs :mutable.Map[String, Double] =
        uniqueTokens.foldLeft(mutable.Map[String, Double]())(
          (tfIdfs :mutable.Map[String, Double], token :String) => {
            tfIdfs += token -> (queryTokenIdfs(token) * tokenFrequencyInQuery(token) / maxTokenFrequencyInQuery)
          }
        )

      val queryLength: Double = getLength(queryTfIdfs.values.toList)

      // TODO: monster block of nonsense
      val tweetTfIdfs :mutable.Map[String, List[Tuple2[String, Double]]] =
        uniqueTokens.foldLeft(mutable.Map[String, List[Tuple2[String, Double]]]())(
          (tfIdfs :mutable.Map[String, Tuple2[String, Double]], token :String) => {
            tweetsByToken(token).foreach((item :Tuple2[Double, String]) => {
              if (tfIdfs.contains(item._2)) tfIdfs(item._2) += Tuple2(item._1, token)
              else tfIdfs += item._2 -> List(Tuple2(item._1, token))
            })
          }
        )

      for (t <- uniqueTokens) {
        for (item <- tweetsByToken(t)) {
          // incrementally compute cosine similarity
          scores.put(item._2, scores.getOrElse(item._2, 0.0) + item._1 * queryTfIdfs(t))
          // TODO: something is going wrong in the calculations
        }
      }

      // sort by score, and return top n results
      scores.map(toResult(_, queryLength)).toSeq.sortWith(_._2 > _._2)
    }

    def toResult (item :(String, Double), qLength :Double) :Tuple2[String, Double] = {
      new Tuple2[String, Double] (item._1, item._2 / (documentLengths(item._1) * qLength))
    }

    getDocumentLengths
    queries.foreach(q => {
      retrieve(q).take(2).foreach(println)
    })
  }
}
