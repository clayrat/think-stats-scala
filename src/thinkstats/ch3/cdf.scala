package thinkstats.ch3

import spire.math._
import spire.math.compat._
import scala.collection._

class Cdf(val xps: List[(Number, Double)]) {

  def values = xps.unzip._1
  def items = xps

  def render = items.map { x => (x._1, Number(x._2)) }
  def renderCCDF = items.map({ x => (x._1, Number(1 - x._2)) })

  def prob(x: Number): Double = if (x < xps.head._1) 0.0 else xps.toSeq.takeWhile(_._1 <= x).last._2

  def value(p: Double): Number = {
    p match {
      case 0 => xps.head._1
      case 1 => xps.last._1
      case p if (p < 0 || p > 1) => throw new IllegalArgumentException("Probability out of [0,1]")
      case p => xps.toSeq.takeWhile(_._2 <= p).last._1
    }
  }

  def percentile(n: Int) = value(n / 100.0)
  def percentileRank(n: Number) = round(prob(n) * 100).toInt

  def median = percentile(50)
  def interquartile = percentile(75) - percentile(25)

  def sample(n: Int): List[Number] = List.fill(n)(value(scala.util.Random.nextDouble()))

}

object Cdf {
  import thinkstats.ch2._

  def fromList(xs: List[Number]): Cdf = {
    fromHist(Hist.fromList(xs))
  }

  def fromHist(h: Hist[Number]): Cdf = {
    fromItems(h.items)
  }

  def fromItems(its: List[(Number, Int)]): Cdf = {
    val (xs, cs) = its.sorted.unzip
    val cumcs = cs.scanLeft(0.0)(_ + _).tail
    new Cdf((xs zip (cumcs map (_ / cumcs.last))))
  }

  def main(args: Array[String]) {

    import thinkstats.ch1._
    import thinkstats.helper.plot._
    import thinkstats.helper.util._

    // 3.6
    val preg = new Pregnancies
    preg.readRecords
    val liveBirths = preg.records.filter(_("outcome") == 1).toList
    val (firstWgt, nonfirstWgt) = map2(liveBirths.partition(_("birthord") == 1), { a: mutable.Map[String, Double] => a("totalwgt_oz") })
    val firstGram = (firstWgt.filterNot(_.isNaN) map ozToGram)
    val firstCdf = Cdf.fromList(firstGram map (Number(_)))
    println(firstCdf.percentileRank(3180))

    // 3.9
    val liveGrams = liveBirths map { _("totalwgt_oz") } filterNot (_.isNaN) map ozToGram
    val liveCdf = Cdf.fromList(liveGrams map (Number(_)))
    val liveSample = liveCdf.sample(1000)
    val sampleCdf = Cdf.fromList(liveSample)
    linePlot2(liveCdf.render, "All live weights CDF", sampleCdf.render, "Sample weights CDF", xtitle = "Grams", ytitle = "P")

    //3.11
    println(liveCdf.percentile(25))
    println(liveCdf.median)
    println(liveCdf.percentile(75))

  }

}