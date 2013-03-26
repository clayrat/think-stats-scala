package ugent

import scala.math._

object thinkstats {

  def mean(as: List[Double]): Double =
    as.sum / as.size

  def vari(as: List[Double]): Double = {
    val mu = mean(as)
    as.map({ a => pow(a - mu, 2) }).sum / as.size
  }
  
  def stddev(as: List[Double]): Double = sqrt(vari(as))

}