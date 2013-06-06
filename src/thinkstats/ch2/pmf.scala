package thinkstats.ch2

import spire.math._
import spire.math.compat._

//import scala.math._

class Hist[A](val histMap: Map[A, Long]) {

  def items = histMap.toList

  def freq(key: A) = histMap.getOrElse(key, 0)

  // 2.3
  def allmodes = items.sortBy(a => a._2).reverse
  def mode = allmodes.head._1

}

object Hist {
  def fromMap[A](map: Map[A, Long]) =
    new Hist(map)

  def fromList[A](as: List[A]) =
    new Hist(as.distinct.map(a => (a, as.count(_ == a).toLong)).toMap)

  def fromItems[A](as: List[(A, Long)]) =
    new Hist(as.groupBy(a => a._1).values.toList map { a: List[(A, Long)] => (a.head._1, a.unzip._2.sum) } toMap)

}

object numHist {

  def binData(as: List[Number], bins: Int): List[Number] = {
    val step = (as.max - as.min) / bins
    (for (n <- 1 to bins) yield {
      val number = as.count { a => as.min + step * (n - 1) < a && a <= as.min + step * n }
      List.fill(number)(as.min + step * (n - 0.5))
    }).toList.flatten
  }

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
  def fromMap[A](map: Map[A, Double]) = {
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

class numPmf(val numMap: Map[Number, Double]) extends Pmf[Number](numMap: Map[Number, Double]) {

  //2.5
  def mean = items.map(a => a._1 * a._2).sum

  def moment(exp: Double): Double = {
    val mu = mean
    items.map(a => a._2 * pow((a._1 - mu).toDouble, exp)).sum
  }

  def variance = moment(2)

  def meanDifference =
    (for ((v1, p1) <- items; (v2, p2) <- items)
      yield math.abs((v1 - v2).toDouble) * p1 * p2).sum

}

object numPmf {

  def fromPmf(pmf: Pmf[Number]): numPmf =
    new numPmf(pmf.pmfMap)

  implicit def pmf2numPmf(pmf: Pmf[Number]) = numPmf.fromPmf(pmf)

}

object PmfTest {

  //2.4
  def remainingLifetime(lifetimes: Pmf[Int], age: Int): Pmf[Int] =
    new Pmf[Int](lifetimes.pmfMap.filterNot { case (key, _) => (key < age) }).normalized

  def main(args: Array[String]) {

    import numPmf._

    val pmf = Pmf.fromList(List(1, 2, 2, 3, 5) map { Number(_) })
    println(pmf.variance)

  }

}