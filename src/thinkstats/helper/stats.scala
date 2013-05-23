package thinkstats.helper

import thinkstats.helper.plot._
import thinkstats.ch2._
import thinkstats.ch3._

import spire.math._
import spire.math.compat._
import spire.math.Number.intToNumber

import scala.math._
import scala.collection.mutable

import thinkstats.ch2.Pmf
import thinkstats.ch3.Cdf

object stats {

  def mean(as: List[Number]): Double =
    (as.sum / as.size).toDouble

  def variance(as: List[Number]): Double = {
    val mu = mean(as)
    as.map({ a => scala.math.pow(a.toDouble - mu, 2) }).sum / as.size
  }

  def stddev(as: List[Number]): Double = sqrt(variance(as))

  def trimmedMeanVar(t: List[Number], p: Double = 0.01): (Double, Double) = {

    def trim(t: List[Number], p: Double = 0.01): List[Number] = {
      /* Trims the largest and smallest elements of t.
     * p: fraction of values to trim off each end
     */
      val n = (p * t.size).toInt
      t.sorted.drop(n).dropRight(n)
    }

    val trimmed = trim(t, p)
    (mean(trimmed), variance(trimmed))
  }

  def median(s: List[Number]) = {
    val (lower, upper) = s.sortWith(_ < _).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }

  // TODO Double->BigInt of some sort?
  def binomCoef(n: Int, k: Int): Double = {
    /* 
     * Compute the binomial coefficient "n choose k".
     *  Args:
     *   n: number of trials
     *   k: number of successes
     */
    var memo: mutable.Map[(Int, Int), Double] = mutable.Map()
    def binomHelper(n: Int, k: Int): Double =
      (n, k) match {
        case (0, k) => 0
        case (n, 1) => 1
        // TODO nicer memoization
        case (n, k) => memo.getOrElse((n, k), {
          val res = binomHelper(n - 1, k) + binomHelper(n - 1, k - 1)
          memo((n, k)) = res
          res
        })
      }
    binomHelper(n, k)
  }

  // from http://picomath.org/scala/Erf.scala.html 
  def erf(x: Double): Double = {
    val a1: Double = 0.254829592
    val a2: Double = -0.284496736
    val a3: Double = 1.421413741
    val a4: Double = -1.453152027
    val a5: Double = 1.061405429
    val p: Double = 0.3275911
    val sign = if (x < 0) -1 else 1
    val absx = math.abs(x)
    val t = 1.0 / (1.0 + p * absx)
    val y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * math.exp(-x * x)
    sign * y
  }

  def main(args: Array[String]) {
    import thinkstats.ch2._
    import thinkstats.ch3._

    //3.10
    val thousand = List.fill(1000)(scala.util.Random.nextDouble())
    val thPmf = Pmf.fromList(thousand)
    val thCdf = Cdf.fromList(thousand map (Number(_)))
    linePlot(thPmf.items.map({ x => (Number(x._1), Number(x._2)) }), "Random 1000 PMF")
    linePlot(thCdf.render, "Random 1000 CDF")
  }

}