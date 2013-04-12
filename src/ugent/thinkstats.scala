package ugent

import spire.math._
import spire.math.compat._

import scala.math._

object thinkstats {

  def mean(as: List[Double]): Double =
    as.sum / as.size

  def variance(as: List[Double]): Double = {
    val mu = mean(as)
    as.map({ a => scala.math.pow(a - mu, 2) }).sum / as.size
  }

  def stddev(as: List[Double]): Double = sqrt(variance(as))

  def main(args: Array[String]) {
    //3.10
    val thousand = List.fill(1000)(scala.util.Random.nextDouble())
    val thPmf = new Pmf(thousand)
    val thCdf = Cdf.fromList(thousand map (Number(_)))
    plot.linePlot(thPmf.items.map({ x => (Number(x._1), Number(x._2)) }), "Random 1000 PMF")
    plot.linePlot(thCdf.render, "Random 1000 CDF")
  }

}