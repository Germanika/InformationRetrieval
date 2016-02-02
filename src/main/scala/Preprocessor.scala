
import java.io.InputStream

import utils.Utils.{ Tokenizer, readFromResource }


object Preprocessor {


  def main (args : Array[String]): Unit = {
    val tokenizr = Tokenizer()
    case class Tweet(time: String, content: Array[String])

    def getTweets: List[Tweet] = {
      readFromResource("/Trec_microblob11.txt")
        .toList
        .foldRight[List[Tweet]](List[Tweet]())(
        (line: String, tweets: List[Tweet]) => {
          val arr: Array[String] = line.split("   ") // why is it 3 spaces???
          if (arr.length >= 2) new Tweet(arr(0), tokenizr(arr(1))) :: tweets
          else {
            tweets
          }
        }
      )
    }

    case class IndexItem(tweet: Tweet, tf: Int)

    def buildIndex( tweets: List[Tweet]):Map[String, IndexItem] = {

    }

    // The inverted index will have the following format:

    try {
      buildIndex
    } catch {
      case e: Exception => println(e)
      case _: Throwable => println("wut")
    }
  }
}
