package ugent

import scala.collection.mutable
import scala.math._

class DictWrapper[A, B] {
  var theMap: mutable.Map[A, B] = mutable.Map()

  def items = theMap.toList
  def values = theMap.keys.toList
}

class Hist[A] extends DictWrapper[A, Int] {

  def this(as: List[A]) = {
    this()
    for (a <- as)
      if (!theMap.contains(a))
        theMap(a) = 1
      else theMap(a) += 1
  }

  def freq(key: A) = theMap.getOrElse(key, 0)

  def allmodes = items.sortBy(a => a._2).reverse
  def mode = allmodes.head._1

}

class Pmf[A] extends DictWrapper[A, Double] {

  def this(map: Map[A, Double]) = {
    this()
    theMap = mutable.Map[A, Double](map.toSeq: _*)
  }

  def this(hist: Hist[A]) = {
    this()
    val total = hist.items.unzip._2.sum
    for ((a, freq) <- hist.items)
      theMap(a) = freq.toDouble / total
  }

  def this(as: List[A]) = {
    this(new Hist[A](as))
  }

  def total = items.unzip._2.sum
  def normalize = {
    val tot = total
    for ((a, prob) <- items) theMap(a) = prob / tot
  }
  def prob(key: A): Double = theMap.getOrElse(key, 0)

}

object PmfTest {

  def pmfMean(pmf: Pmf[Int]) = pmf.items.map(a => a._1 * a._2).sum
  def pmfVari(pmf: Pmf[Int]) = {
    val mu = pmfMean(pmf)
    pmf.items.map(a => a._2 * pow(a._1 - mu, 2)).sum
  }

  def remainingLifetime(lifetimes: Pmf[Int], age: Int): Pmf[Int] = {
    val newPmf = new Pmf[Int](lifetimes.items.filterNot(a => (a._1 < age)).toMap)
    newPmf.normalize
    newPmf
  }

  def main(args: Array[String]) {
    val pmf = new Pmf[Int](List(1, 2, 2, 3, 5))
    println(pmfVari(pmf))

  }

}