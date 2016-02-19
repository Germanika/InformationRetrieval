
import java.io.FileWriter

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
      math.sqrt(values.foldRight(0.0)((x :Double, accum :Double) =>
        accum + math.pow(x, 2)))
    }

    // list of tweets and their tf-idf values from a token
    def tweetsByToken (token :String) :List[Tuple2[Double, String]] = {
      val termIdf = tokenIdf(token)
      val result = Index.invertedIndex(token).toList.map(item => {
        Tuple2(item._2 * termIdf, item._1)
      })
      result
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

    def retrieve (query :Query, queryNum :Int, numResults :Int) :Seq[(Int, String, Double)] = {
//      val scores = mutable.HashMap[String, Double]()
      val tokens = query.tokens.filter( (t :String) => Index.invertedIndex.contains(t))
      val tokenFrequencyInQuery = tokens.foldRight(mutable.Map[String, Int]())( (token :String, frequencies :mutable.Map[String, Int]) => {
        frequencies += token -> (frequencies.getOrElse(token, 0) + 1)
      })

      val maxTokenFrequencyInQuery :Int = tokenFrequencyInQuery.foldRight(0)(
        (item :(String, Int), max :Int) => {
          if (max > item._2) max
          else item._2
        }
      )

      val uniqueTokens = tokens.toSet

      val queryTokenIdfs :mutable.Map[String, Double] =
        uniqueTokens.foldRight(mutable.Map[String, Double]())(
          (token :String, idfs :mutable.Map[String, Double]) => {
            idfs += token -> tokenIdf(token)
          }
        )

      val queryTfIdfs :mutable.Map[String, Double] =
        uniqueTokens.foldRight(mutable.Map[String, Double]())(
          (token :String, tfIdfs :mutable.Map[String, Double]) => {
            tfIdfs += token -> (queryTokenIdfs(token) * tokenFrequencyInQuery(token) / maxTokenFrequencyInQuery)
          }
        )


      // tweetId -> [ (tfidf, token), (tfidf, token), ... ]
      val tweetTfIdfs :mutable.Map[String, List[(Double, String)]] =
        uniqueTokens.foldRight(mutable.Map[String, List[(Double, String)]]())(
          (token :String, tfIdfs :mutable.Map[String, List[(Double, String)]]) => {
            tweetsByToken(token).foreach(
              (item :(Double, String)) => {
                if (tfIdfs.contains(item._2)) Tuple2(item._1, token) :: tfIdfs(item._2)
                else tfIdfs += item._2 -> List(Tuple2(item._1, token))
              }
            )
            tfIdfs
          }
        )

      // tid -> tweetLength
      val tweetLengths :mutable.Map[String, Double] =
        tweetTfIdfs.keys.foldRight(
          mutable.Map[String, Double]())(
          (tid :String, lengths :mutable.Map[String, Double]) => {
            lengths += tid -> getLength(tweetTfIdfs(tid).map(_._1))
          })

      val queryLength: Double = getLength(queryTfIdfs.values.toList)

      // tid -> score
      val similarityScores :mutable.Map[String, Double] =
        tweetTfIdfs.keys.foldRight( mutable.Map[String, Double]() )(
          (tid :String, scores :mutable.Map[String, Double]) => {
           scores += tid ->
             (tweetTfIdfs(tid).foldRight(0.0)(
               (item :(Double, String), acc :Double) =>
                 acc + item._1 * queryTfIdfs(item._2)
               ) / (tweetLengths(tid) * queryLength))
          })

      def toResult(result :(String, Double)) :(Int, String, Double) = {
        Tuple3(queryNum, result._1, result._2)
      }

      // sort by score, take top numResults results
      similarityScores.map(toResult).toSeq.sortWith(_._3 > _._3).take(numResults)
    }

    val fw = new FileWriter("Results", true)
    var count = 0
    queries.map(q => {
      count += 1
      retrieve(q, count, 1000)
    }).map((results :Seq[(Int, String, Double)]) => {
      var rank = 0
      results.foreach(
        (result :(Int, String,Double)) => {
          rank += 1
          fw.write(
            result._1 +
              " Q0 " +
              result._2 + " " +
              rank + " " +
              result._3 + " IanGermann_6979433 \n"
          )
        }
      )
    })
    fw.close()
  }
}
