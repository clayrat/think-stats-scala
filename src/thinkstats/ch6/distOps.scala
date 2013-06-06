package thinkstats.ch6

import thinkstats.helper.stats._

import spire.math._
import spire.math.compat._

import scala.util.Random

import thinkstats.ch2._
import thinkstats.ch2.numPmf._

// 6.4
class Gumbell(mu: Double, beta: Double) {
  def generate = mu - beta * log(-log(Random.nextDouble))
}

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
    println("Pregnancy lengths:")
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

  def gumbell {
    import thinkstats.ch3._
    import thinkstats.helper.plot._
    val gum1 = new Gumbell(0.5, 2.0)
    val gum1Cdf = Cdf.fromList(List.fill(1000)(gum1.generate))
    val gum2 = new Gumbell(3.0, 4.0)
    val gum2Cdf = Cdf.fromList(List.fill(1000)(gum2.generate))
    linePlot2(gum1Cdf.render, "mu=0.5, b=2.0", gum2Cdf.render, "mu=3.0, b=4.0")
  }

  // 6.5
  def exp1to20(x: Double) = exp(-x) - exp(-20 * x)

  // 6.6
  // 5'10" = 177.8 cm
  // 6'1" = 185.4 cm
  def blueMan {
    import thinkstats.helper.stats._
    def normCDF(mu: Double, sig: Double)(x: Double) = (1 + erf((x - mu) / (sig * math.sqrt(2)))) / 2
    def popCDF = normCDF(178.0, math.sqrt(59.4)) _
    println("Eligible for Blue Man are: " + 100 * (popCDF(185.4) - popCDF(177.8)) + "%")
  }

  // 6.7
  // it's l^3*exp(-l*x)*x^2/2, which is Erlang distribution, k=3

  // 6.8-9
  def functionsOnPMFs(am: Int, bm: Int) = {
    def support2F(xp: Pmf[Int], yp: Pmf[Int], f: (Int, Int) => Int) =
      (for (x <- xp.vals; y <- yp.vals) yield f(x, y)).distinct

    def sumPMFs(xp: Pmf[Int], yp: Pmf[Int]): Pmf[Int] =
      Pmf.fromMap(
        (for (z <- support2F(xp, yp, { _ + _ }))
          yield (z, yp.vals.map(y => xp.prob(z - y) * yp.prob(y)).sum))
          .toMap)

    def maxPMFs(xp: Pmf[Int], yp: Pmf[Int]): Pmf[Int] =
      Pmf.fromMap(
        (for (z <- support2F(xp, yp, max))
          yield (z,
          (for (x <- xp.vals if x <= z) yield xp.prob(x) * yp.prob(z)).sum
          + (for (y <- yp.vals if y < z) yield xp.prob(z) * yp.prob(y)).sum))
          .toMap)
          
    val a = Pmf.fromList((0 to am).toList)
    val b = Pmf.fromList((0 to bm).toList)
    println("[0," + am + "]+[0," + bm + "]:")
    println(sumPMFs(a, b).items.sorted)
    println("max([0," + am + "],[0," + bm + "]):")
    println(maxPMFs(a, b).items.sorted)
  }

  def main(args: Array[String]) {
    pregnancySkewness
    incomeStats
    gumbell
    blueMan
    functionsOnPMFs(1, 1)
  }

}