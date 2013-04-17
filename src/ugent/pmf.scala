package ugent

import spire.math._
import spire.math.compat._

import scala.math._

class Hist[A](val histMap: Map[A, Int]) {

  def items = histMap.toList

  def freq(key: A) = histMap.getOrElse(key, 0)

  // 2.3
  def allmodes = items.sortBy(a => a._2).reverse
  def mode = allmodes.head._1

}

object Hist {
  def fromMap[A](map: Map[A, Int]) =
    new Hist(map)

  def fromList[A](as: List[A]) =
    new Hist(as.distinct.map(a => (a, as.count(_ == a))).toMap)

}

class Pmf[A](val pmfMap: Map[A, Double]) {

  def items = pmfMap.toList
  def vals = pmfMap.keys.toList

  def total = pmfMap.values.sum
  def prob(key: A): Double = pmfMap.getOrElse(key, 0)

  def multiplied(fun: A => Double): Pmf[A] =
    new Pmf[A](pmfMap.map({ case (a, prob) => (a, prob * fun(a)) }))

  def normalized: Pmf[A] = {
    val tot = total
    new Pmf[A](pmfMap.map({ case (a, prob) => (a, prob / tot) }))
  }

}

object Pmf {
  def fromMap[A](map: Map[A, Double]) {
    new Pmf(map)
  }

  def fromHist[A](hist: Hist[A]) = {
    val total = hist.histMap.values.sum
    new Pmf(hist.histMap.map({ case (a, freq) => (a, freq.toDouble / total) }))
  }

  def fromList[A](as: List[A]) = {
    fromHist(Hist.fromList(as))
  }

}

class intPmf(val intMap: Map[Int, Double]) extends Pmf[Int](intMap: Map[Int, Double]) {
  
  //2.5
  def mean = items.map(a => a._1 * a._2).sum

  def variance: Double = {
    val mu = mean
    items.map(a => a._2 * math.pow(a._1 - mu, 2)).sum
  }

}

object intPmf {

  def fromPmf(pmf: Pmf[Int]): intPmf =
    new intPmf(pmf.pmfMap)

}

object PmfTest {

  implicit def pmf2intPmf(pmf: Pmf[Int]) = intPmf.fromPmf(pmf)
  
  //2.4
  def remainingLifetime(lifetimes: Pmf[Int], age: Int): Pmf[Int] =
    new Pmf[Int](lifetimes.pmfMap.filterNot { case (key, _) => (key < age) }).normalized

  def main(args: Array[String]) {
    val pmf = Pmf.fromList(List(1, 2, 2, 3, 5))
    println(pmf.variance)

  }

}