package thinkstats.ch4

import thinkstats.helper.plot._
import thinkstats.helper.util._
import thinkstats.ch3._

import scalax.io._
import spire.math._
import spire.math.compat._

object irs {
  def readIrsData = {
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
        Some(data.head, data.last.toLong)
      } else None
    }).toArray.flatten
  }

  def incomeList(data: Array[(String, Long)]): List[(Number, Long)] = {
    /* Converts the strings from the IRS file to items list.
     * Args:
     * data: list of (dollar range midpoint, number of returns) string pairs
      */
    def toRange(s: String): (Long, Long) = {
      val splitted = s.split(' ')
      (splitted.head, splitted.last) match {
        case (l, h) if h == "income" => (0, 0)
        case (l, h) if h == "more" => (l.tail.toLong, -1)
        case (l, h) => (l.tail.toLong, h.tail.toLong)
      }
    }

    def midpoint(lowhi: (Long, Long)): Double = lowhi match {
      /*Finds the midpoint of a range.*/
      case (low, high) =>
        if (high == -1)
          low * 3 / 2.0
        else
          (low + high) / 2.0
    }

    data map { case (range, number) => (Number(midpoint(toRange(range))), number) } toList

  }

  def main(args: Array[String]) {
    val data = readIrsData
    val incomeCdf = Cdf.fromItems(incomeList(data))
    linePlot(incomeCdf.render, "Income CDF")
    linePlot(removeFirstZeroes(incomeCdf.render), "Income log CDF", xtitle = "income", logX = true)
    linePlot(removeLastZero(removeFirstZeroes(incomeCdf.renderCCDF)), "Income log log CCDF", xtitle = "income", logX = true, logY = true)
  }

}