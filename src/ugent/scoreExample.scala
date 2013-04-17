package ugent

object scoreExample {

  def percentileRank(scores: List[Int], yrScore: Int) =
    100.0 * scores.filter(_ <= yrScore).size / scores.size

  def percentileSlow(scores: List[Int], rank: Double) =
    scores.sorted.view.filter(percentileRank(scores, _) >= rank).head

  //3.3  
  def percentileBetter(scores: List[Int], rank: Double) = {
    val sortedSc = scores.sorted
    sortedSc((rank * (scores.size - 1) / 100).toInt)
  }

  def main(args: Array[String]) {
    val scores = List(55, 66, 77, 88, 99)
    val yrScore = 88
    println(percentileRank(scores, yrScore))
    println(scores.map(percentileRank(scores, _)))

    val ranks = List(0, 20, 25, 40, 50, 60, 75, 80, 100)
    println(ranks.map(percentileSlow(scores, _)))
    println(ranks.map(percentileBetter(scores, _)))
  }

}