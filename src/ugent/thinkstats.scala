package ugent

import spire.math._
import spire.math.compat._

import scala.math._

object thinkstats {

  def mean(as: List[Number]): Double =
    (as.sum / as.size).toDouble

  def variance(as: List[Number]): Double = {
    val mu = mean(as)
    as.map({ a => scala.math.pow(a.toDouble - mu, 2) }).sum / as.size
  }

  def stddev(as: List[Number]): Double = sqrt(variance(as))

  def trim(t: List[Number], p: Double = 0.01): List[Number] = {
    /* Trims the largest and smallest elements of t.
     * p: fraction of values to trim off each end
     */
    val n = (p * t.size).toInt
    t.sorted.drop(n).dropRight(n)
  }

  def trimmedMeanVar(t: List[Number], p: Double = 0.01): (Double, Double) = {
    val trimmed = trim(t, p)
    (mean(trimmed), variance(trimmed))
  }

  def main(args: Array[String]) {
    //3.10
    val thousand = List.fill(1000)(scala.util.Random.nextDouble())
    val thPmf = Pmf.fromList(thousand)
    val thCdf = Cdf.fromList(thousand map (Number(_)))
    plot.linePlot(thPmf.items.map({ x => (Number(x._1), Number(x._2)) }), "Random 1000 PMF")
    plot.linePlot(thCdf.render, "Random 1000 CDF")
  }

}