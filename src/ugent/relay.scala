package ugent

import spire.math._
import spire.math.compat._

import scala.math._
import scalax.io._

object relay {

  def renderPmf(pmf: Pmf[Double]): List[(Number, Number)] = pmf.items.map { case (x, p) => (Number(x), Number(p)) }

  //3.2
  def biasSpeedPmf(pmf: Pmf[Double], speed: Double): Pmf[Double] =
    pmf.multiplied { a: Double => math.abs(a - speed) }.normalized

  def readPaces = {
    val inp = Resource.fromFile("data/Apr25_27thAn_set1.dat").lines()
    val items = inp.filterNot(_.trim.isEmpty).map(_.split(' ').filterNot(_.isEmpty))
    val properItems = items.filter(e => e(1).contains('/') && e.slice(3, 6).map(_.contains(':')).reduce(_ && _))
    properItems.map(_(5)).toList
  }

  def convertPaces(paces: List[String]) = {
    paces.map(_.split(':').map(_.toInt).toList).map(a => 60.0 * 60.0 / ((a(0) * 60 + a(1))))
  }

  def main(args: Array[String]) {
    val paces = convertPaces(readPaces)

    val pmf = Pmf.fromList(paces)
    plot.linePlot(renderPmf(pmf), "Paces PMF")
    val biasedPmf = biasSpeedPmf(pmf, 7.5)
    plot.histPlot(renderPmf(biasedPmf), "Observed speed")

    //3.5
    val cdf = Cdf.fromList(paces.map(Number(_)))
    plot.linePlot(cdf.render, "Paces CDF")
  }

}