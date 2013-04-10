package ugent

import scala.math._
import scalax.io._

object relay {

  def biasSpeedPmf(pmf: Pmf[Double], speed: Double): Pmf[Double] = {
    val biased = pmf.copy
    biased.items map { a => biased.mult(a._1, abs(a._1 - speed)) }
    biased.normalize
    biased
  }

  def readPaces = {
    val inp = Resource.fromFile("data/Apr25_27thAn_set1.dat").lines()
    val items = inp.filter(!_.trim.isEmpty).map(_.split(' ').filterNot(_.isEmpty))
    val properItems = items.filter(e => e(1).contains('/') && e.slice(3, 6).map(_.contains(':')).reduce(_ && _))
    properItems.map(_(5)).toList
  }

  def convertPaces(paces: List[String]) = {
    paces.map(_.split(':').map(_.toInt).toList).map(a => 60.0 * 60.0 / ((a(0) * 60 + a(1))))
  }

  def main(args: Array[String]) {
    val pmf = new Pmf[Double](convertPaces(readPaces))
    plot.linePlot(pmf, "Paces")
    val biasedPmf = biasSpeedPmf(pmf,7.5)
    plot.histPlot(biasedPmf,"Observed speed")
  }

}