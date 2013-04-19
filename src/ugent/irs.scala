package ugent

import scalax.io._
import spire.math._
import spire.math.compat._

object irs {
  def readData = {
    def process(s: Array[String], word: String): Array[String] =
      {
        if (s.isEmpty) Array()
        else
          s.head match {
            case w if w.head == '"' && w.last == '"' => w.tail.dropRight(1) +: process(s.tail, "")
            case w if w.head == '"' => process(s.tail, w.tail)
            case w if w.last == '"' => (word + w.dropRight(1)) +: process(s.tail, "")
            case w if !word.isEmpty => process(s.tail, word + w)
            case w if word.isEmpty => w +: process(s.tail, "")
          }
      }
    val lines = Resource.fromFile("data/08in11si.csv").lines().filterNot(_.isEmpty)
    (for (line <- lines) yield {
      val raw = line.split(',')
      if (!raw.head.contains("All returns") && !raw.head.contains("Accumulated")) {
        val data = process(raw, "").toList.take(2)
        Some(data.head, data.last.toInt)
      } else None
    }).toArray.flatten
  }

  def makeIncomeDist(data: Array[(String, Int)]): Cdf = {
    /* Converts the strings from the IRS file to Cdf.
     * Args:
     * data: list of (dollar range, number of returns) string pairs
      */
    def toRange(s: String): (Int, Int) = {
      val splitted = s.split(' ')
      (splitted.head, splitted.last) match {
        case (l, h) if h == "income" => (0, 0)
        case (l, h) if h == "more" => (l.tail.toInt, -1)
        case (l, h) => (l.tail.toInt, h.tail.toInt)
      }
    }

    def midpoint(lowhi: (Int, Int)): Double = lowhi match {
      /*Finds the midpoint of a range.*/
      case (low, high) =>
        if (high == -1)
          low * 3 / 2.0
        else
          (low + high) / 2.0
    }

    Cdf.fromItems((data map { case (range, number) => (Number(midpoint(toRange(range))), number) } toList))

  }

  def main(args: Array[String]) {
    val data = readData
    val incomeCdf = makeIncomeDist(data)
    plot.linePlot(incomeCdf.render, "Income CDF")
    plot.linePlot(util.removeFirstZeroes(incomeCdf.render), "Income log CDF", xtitle = "income", logX = true)
    plot.linePlot(util.removeLastZero(util.removeFirstZeroes(incomeCdf.renderCCDF)), "Income log log CCDF", xtitle = "income", logX = true, logY = true)
  }

}