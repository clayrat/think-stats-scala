package ugent

import scala.collection.mutable
import scala.math._

class DictWrapper[A, B] {
  var theMap: mutable.Map[A, B] = mutable.Map()
  
  def items = theMap.toList
  def values = theMap.keys.toList
}

class Hist[A] extends DictWrapper[A, Int] {

  def this(map: Map[A, Int]) {
    this()
    theMap = mutable.Map[A, Int](map.toSeq: _*)
  }

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

  def this(map: Map[A, Double]) {
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
  
  def copy: Pmf[A] = new Pmf[A](items.toMap)
  def mult(key: A, factor: Double) = theMap(key) *= factor 
  def total = items.unzip._2.sum
  def normalize = {
    val tot = total
    for ((a, prob) <- items) theMap(a) = prob / tot
  }
  def prob(key: A): Double = theMap.getOrElse(key, 0)

}

class intPmf extends Pmf[Int] {

  def this(pmf: Pmf[Int]) {
    this()
    theMap = pmf.theMap
  }

  def mean = items.map(a => a._1 * a._2).sum

  def variance = {
    val mu = mean
    items.map(a => a._2 * pow(a._1 - mu, 2)).sum
  }

}

object PmfTest {

  implicit def pmf2intPmf(pmf: Pmf[Int]) = new intPmf(pmf)

  def remainingLifetime(lifetimes: Pmf[Int], age: Int): Pmf[Int] = {
    val newMap = lifetimes.items.filterNot(a => (a._1 < age)).toMap
    val newPmf = new Pmf[Int](newMap)
    newPmf.normalize
    newPmf
  }

  def main(args: Array[String]) {
    val pmf = new Pmf[Int](List(1, 2, 2, 3, 5))
    println(pmf.variance)

  }

}