package ugent

import spire.math._
import spire.math.compat._

class Cdf(val xps: List[(Number, Double)]) {

  def values = xps.unzip._1
  def items = xps

  def prob(x: Number): Double = if (x < xps.head._1) 0.0 else xps.toSeq.takeWhile(_._1 <= x).last._2

  def value(p: Double): Number = {
    p match {
      case 0 => xps.head._1
      case 1 => xps.last._1
      case p if (p < 0 || p > 1) => throw new IllegalArgumentException("Probability out of [0,1]")
      case p => xps.toSeq.takeWhile(_._2 <= p).last._1
    }
  }

}

object Cdf {
  def fromList(xs: List[Number]): Cdf = {
    fromHist(new Hist[Number](xs))
  }

  def fromHist(h: Hist[Number]): Cdf = {
    fromItems(h.items)
  }

  def fromItems(its: List[(Number, Int)]): Cdf = {
    val (xs, cs) = its.sorted.unzip
    val cumcs = cs.scanLeft(0.0)(_ + _).tail
    new Cdf((xs zip (cumcs map (_ / cumcs.sum))))
  }

}