import utils.Utils.{ Tokenizer, readFromResource }

class Index {

  var tweets = Map[String, String]() // tweetID -> actual tweet
  var invertedIndex = Map[String, Map[String, Int]]() // token -> (tweetID -> tf)
  var documentFrequency = Map[String, Int]() // token -> documentFrequency

  val tokenizr = Tokenizer()

  tweets = getTweets

  def getTweets: Map[String, String] = {
    readFromResource("/Trec_microblog11.txt")
      .toList
      .foldRight( Map[String, String]() )(
        (line: String, tweets: Map[String, String]) => {
          val arr: Array[String] = line.split("\t")
          if (arr.length == 2) {
            addToInvertedIndex(arr(0), arr(1))
            tweets + (arr(0) -> arr(1))
          } else tweets
        }
      )
  }

  def addToInvertedIndex(id: String, content: String): Unit = {
    tokenizr(content).foreach(f = (token: String) => {
      // get map item
      // if it already exists, try to get the tweet which has it
      // if that tweet is in the map, increase its tf
      // otherwise, add the tweet to the array, and increase the df

      if (invertedIndex.contains(token)) {
        var tweets: Map[String, Int] = invertedIndex.getOrElse(token, Map[String, Int]())

        if (tweets.contains(id)) {
          documentFrequency += (token -> (documentFrequency.getOrElse(token, 0) + 1))
        }

        // increment tf
        var tf: Int = tweets.getOrElse(id, 0) + 1
        tweets += (id -> tf)

        invertedIndex += token -> tweets
      } else {

        // if it's nowhere to be found, add new entry to index
        invertedIndex += (token -> Map(id -> 1))
        documentFrequency += (token -> 1)
      }
    })
  }
}
