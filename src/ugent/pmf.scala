package ugent

import scala.collection.mutable

class Hist[A] {
  var histMap: mutable.Map[A, Int] = mutable.Map()

  def makeHistFromList(as: List[A]) = {
    for (a <- as)
      if (!histMap.contains(a))
        histMap(a) = 1
      else histMap(a) += 1
  }

  def freq(key: A) = histMap.getOrElse(key, 0)
  def items = histMap.toList
  def values = histMap.keys.toList
  
  def allmodes = items.sortBy(a => a._2).reverse
  def mode = allmodes.head._1
}

class Pmf {

}

object PmfTest {
  def main(args: Array[String]) {
    val hist = new Hist[Int]
    hist.makeHistFromList(List(1, 2, 2, 3, 5))
  }

}