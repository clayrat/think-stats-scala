package thinkstats.ch4

import thinkstats.helper.plot._
import thinkstats.helper.util._
import thinkstats.ch3._

import scalax.io._
import java.util.zip.GZIPInputStream
import java.io.FileInputStream

import spire.math._
import spire.math.compat._

object populations {

  def readData = {
    def process(s: Array[String], word: String): Array[String] =
      {
        if (s.isEmpty) Array()
        else
          s.head match {
            case w if w.head == '"' => process(s.tail, w.tail)
            case w if w.last == '"' => (word + w.dropRight(1)) +: process(s.tail, "")
            case w if !word.isEmpty => process(s.tail, word + w)
            case w if word.isEmpty => w +: process(s.tail, "")
          }
      }
    val lines = Resource.fromInputStream(new GZIPInputStream(new FileInputStream("data/populations.csv.gz"))).lines().filterNot(_.isEmpty)
    (for (line <- lines) yield {
      val raw = line.split(',')
      if (raw.head.contains("city") || raw.head.contains("town")) {
        val prev = process(raw, "").dropRight(1).last
        if (!(prev == "-")) Some(prev.toInt) else None
      } else None
    }).toArray.flatten
  }

  def main(args: Array[String]) {
    val cities = readData.toList.map { Number(_) }
    println(cities.size)
    
    //4.12
    val citiesCdf = Cdf.fromList(cities)
    linePlot(citiesCdf.render, "City/Town Populations CDF", xtitle = "population")
    linePlot(citiesCdf.render, "City/Town Populations CDF log", logX = true, xtitle = "population log")
    linePlot(removeLastZero(citiesCdf.renderCCDF), "City/Town Populations CCDF log log", logX = true, logY = true, xtitle = "population log", ytitle = "P log")
    continuous.normalPlot(cities, title = "Population normal plot", ytitle = "population")
    continuous.normalPlot(cities.map(_.toDouble).map(math.log).map { Number(_) }, title = "Population log normal plot", ytitle = "log(population)")
  }

}