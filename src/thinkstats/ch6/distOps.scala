package thinkstats.ch6

import thinkstats.helper.stats._

import spire.math._
import spire.math.compat._

import thinkstats.ch2._
import thinkstats.ch2.numPmf._

object distOps {

  // 6.1
  def pregnancySkewness {
    import thinkstats.ch1.Pregnancies
    import thinkstats.helper.util._

    def skewness(sample: List[Number]): Double = {
      def m(as: List[Number], i: Int) = (as map { a: Number => pow(a.toDouble, i) } sum) / as.size
      val deviation = sample map { _ - mean(sample) }
      m(deviation, 3) / pow(m(deviation, 2), 1.5)
    }
    def pearsonSkewness(sample: List[Number]) = 3 * (mean(sample) - median(sample)) / stddev(sample)

    val preg = new Pregnancies
    preg.readRecords
    val liveBirths = preg.records.filter(_("outcome") == 1).toList
    val weights = liveBirths map { _("totalwgt_oz") } filterNot (_.isNaN) map { a => Number(ozToGram(a)) }
    val lengths = liveBirths map { a => Number(a("prglength")) }
    println("= Pregnancy lengths =")
    println("skewness = " + skewness(lengths) + ", pearson-sk = " + pearsonSkewness(lengths))
    println("Birth weights:")
    println("skewness = " + skewness(weights) + ", pearson-sk = " + pearsonSkewness(weights))
  }

  def incomeStats {
    // 6.3
    import thinkstats.ch3._
    import thinkstats.ch4._

    def gini(pmf: Pmf[Number]) = pmf.meanDifference / (2 * pmf.mean)

    val items = irs.incomeList(irs.readIrsData)

    val incPmf = Pmf.fromHist(Hist.fromItems(items))
    val incCdf = Cdf.fromItems(items)
    val incMean = incPmf.mean
    val incVar = incPmf.variance
    val incMedian = incCdf.median
    println("= Income statistics =")
    println("Fraction below the mean is " + incCdf.prob(incMean))
    println("Median is " + incMedian)
    println("Mean is " + incMean)
    println("Skewness is " + incPmf.moment(3) / pow(incVar, 1.5))
    println("Pearson-sk is " + 3 * (incMean - incMedian) / math.sqrt(incVar))
    println("Gini index is " + gini(incPmf))
  }

  def main(args: Array[String]) {
    pregnancySkewness
    incomeStats
  }

}