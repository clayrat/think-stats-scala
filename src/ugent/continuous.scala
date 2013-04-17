package ugent

import scalax.io._
import java.util.zip.GZIPInputStream
import java.io.FileInputStream

import jsc.distributions._

import spire.math._
import spire.math.compat._

object continuous {

  def renderPmf(pmf: Pmf[Double]): List[(Number, Number)] = pmf.items.map { case (x, p) => (Number(x), Number(p)) }
  def removeZero(a: List[(Number, Number)]) = a.dropRight(1) :+ (a.last._1, a.dropRight(1).last._2)

  def babyInterarrival {
    val babyMinutes = List(5, 64, 78, 115, 177, 245, 247, 262, 271, 428, 455, 492, 494, 549, 635, 649, 653, 693, 729, 776, 785, 846, 847, 873, 886, 914, 991, 1017, 1062, 1087, 1105, 1134, 1149, 1187, 1189, 1191, 1210, 1237, 1251, 1264, 1283, 1337, 1407, 1435)
    val intertimes = (babyMinutes.drop(1) zip babyMinutes.dropRight(1)) map { case (a, b) => a - b }
    val babyCdf = Cdf.fromList(intertimes map { Number(_) })
    plot.linePlot(babyCdf.render, "interrival times CDF")
    plot.linePlot(removeZero(babyCdf.renderCCDF), "interrival times CCDF log", logY = true)
  }

  // 4.1
  def exponentialTask {
    val expDist = new Exponential(32.6)
    val expList = List.fill(44)(expDist.random)
    val expCdf = Cdf.fromList(expList map { Number(_) })
    plot.linePlot(removeZero(expCdf.renderCCDF), "exponential samples CCDF log", logY = true)
  }

  // 4.3
  def paretoSamples {
    val parDist = new Pareto(0.5, 1.0)
    val parList = List.fill(100)(parDist.random)
    val parCdf = Cdf.fromList(parList map { Number(_) })
    plot.linePlot(removeZero(parCdf.renderCCDF), "Pareto samples CCDF log:log", logX = true, logY = true)
  }

  //4.4.
  //no can do 6E9 (World in 2000) easily, gotta settle with 4E7 (Spain in 2000)
  def paretoSpain {
    val parSpainDist = new Pareto(100.0, 1.7)
    val parSpain = Array.fill(40000000)(parSpainDist.random)
    val parSpainMean = parSpain.sum / parSpain.size
    println("Pareto Spain mean: " + parSpainMean)
    println("Pareto Spain short fraction: " + parSpain.filter(_ < parSpainMean).size.toDouble / parSpain.size)
    println("Pareto Spain max: " + parSpain.max)
  }

  //4.5
  def zipfLaw {
    val punctuation = Set('-', ',', '.', '!', '?', '_', '*', '(', ')')
    val lines = Resource.fromInputStream(new GZIPInputStream(new FileInputStream("data/pg4300.txt.gz"))).lines().filterNot(_.isEmpty)
    val wordSize = lines.map(_.filterNot(punctuation.contains(_)).split(' ').filterNot(_.isEmpty).map(_.size)).toList.flatten
    val wordCdf = Cdf.fromList(wordSize map { Number(_) })
    plot.linePlot(removeZero(wordCdf.renderCCDF), "Ulysses word size CCDF log:log", logX = true, logY = true, xtitle = "word size")
  }

  //4.6
  def weibullTransform {
    def axisTransform(a: List[(Number, Number)]): List[(Number, Number)] = a map { case (x, prob) => (x, -log(1 - prob)) }
    val weiDist = new Weibull(1.0, 1.5)
    val weiList = List.fill(100)(weiDist.random)
    val weiCdf = Cdf.fromList(weiList map { Number(_) })
    plot.linePlot(removeZero(axisTransform(weiCdf.render)), "Weibull sample CDF log:log(-log(1-F(X)))", logX = true, logY = true, ytitle = "-log(1-Y)")
  }

  //4.7
  def WAIS {
    val iqDist = new Normal(100.0, 15.0)
    val iqList = Array.fill(40000000)(iqDist.random) // Spain again
    println("IQ fraction >100: " + iqList.filter(_ > 100.0).size.toDouble / iqList.size)
    println("IQ fraction >115: " + iqList.filter(_ > 115.0).size.toDouble / iqList.size)
    println("IQ fraction >130: " + iqList.filter(_ > 130.0).size.toDouble / iqList.size)
    println("IQ fraction >145: " + iqList.filter(_ > 145.0).size.toDouble / iqList.size)
    println("IQ number >190: " + iqList.filter(_ > 190.0).size)
  }

  //4.8
  def normalPregnancy {
    val table = new Pregnancies
    table.readRecords
    val (live, _, _) = util.map3(first.liveFirstNonFirst(table), { a: Double => Number(a) })
    val (mu, sig) = (thinkstats.mean(live), thinkstats.stddev(live))

    val liveModelDist = new Normal(mu, sig)
    val liveModel = List.fill(10000)(liveModelDist.random)

    val liveCdf = Cdf.fromList(live)
    val liveModelCdf = Cdf.fromList(liveModel map { Number(_) })

    plot.linePlot2(liveCdf.render, "Live pregnancies length CDF", liveModelCdf.render, "Normal dist model CDF", xtitle = "Weeks", ytitle = "P")
  }

  //4.9
  def samples() {
    def sample: List[Double] = {
      val normDist = new Normal(0, 1)
      List.fill(6)(normDist.random).sorted
    }
    println(List.fill(1000)(sample).transpose.map { l => thinkstats.mean(l.map { Number(_) }) })
  }

  //4.10
  def normalPlot(ys: List[Number], title: String = "Y") {
    val normDist = new Normal(0, 1)
    val xs = List.fill(ys.size)(Number(normDist.random)).sorted
    plot.scatterPlot(xs zip ys.sorted, "Normal plot", xtitle = "Standard normal values", title)
  }

  def main(args: Array[String]) {
    babyInterarrival
    exponentialTask
    paretoSamples
    paretoSpain
    zipfLaw
    weibullTransform
    WAIS
    normalPregnancy
  }

}