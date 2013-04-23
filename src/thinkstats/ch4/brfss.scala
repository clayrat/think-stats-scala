package thinkstats.ch4

import thinkstats.helper.stats._
import thinkstats.helper.plot._
import thinkstats.ch1._
import thinkstats.ch3._

import spire.math._
import spire.math.compat._
import scala.Array.canBuildFrom

class BRFSSRespondents extends Table {
  val filename = "data/BRFSlight.ASC.gz" // first 15000 lines of CDBRFS08.ASC gzipped 
  val fields = List(
    ("age", 101, 102),
    ("weight2", 119, 122),
    ("wtyrago", 127, 130),
    ("wtkg2", 1254, 1258),
    ("htm3", 1251, 1253),
    ("sex", 143, 143))

  def recode {

    def cleanWeight(weight: Int) = weight match {
      case x if (Set(7777, 9999).contains(x)) => Double.NaN
      case x if (x < 1000) => x / 2.2
      case x if (9001 to 9998).contains(x) => x - 9000
      case _ => weight
    }

    for (rec <- records) {
      // recode wtkg2
      if (rec("wtkg2").isNaN || rec("wtkg2") == 99999)
        rec("wtkg2") = Double.NaN
      else
        rec("wtkg2") /= 100.0
      // recode wtyrago
      rec("weight2") = cleanWeight(rec("weight2").toInt)
      rec("wtyrago") = cleanWeight(rec("wtyrago").toInt)
      // recode htm3
      if (rec("htm3") == 999)
        rec("htm3") = Double.NaN
      // recode age
      if (Set(7, 9).contains(rec("age").toInt))
        rec("age") = Double.NaN
    }
  }

  def summarizeParam(param: String) {
    /* 
     * Print summary statistics for male and female given parameter 
     */

    def summary(t: List[Double]) {
      val (mu, vari) = trimmedMeanVar(t map { Number(_) })
      val sigma = math.sqrt(vari)
      val cv = sigma / mu
      List(t.size, mu, vari, sigma, cv) map { d => print(d + " ") }
      println
    }

    val male = records.filter(_("sex") == 1).map { _(param) }.filterNot(_.isNaN).toList
    val female = records.filter(_("sex") == 2).map { _(param) }.filterNot(_.isNaN).toList
    val all = male ++ female
    println("gen n        mean               var                sigma                cv")
    print("mal ")
    summary(male)
    print("fem ")
    summary(female)
    print("all ")
    summary(all)
  }

  def summarizeWeightChange {
    /* 
     * Print the mean reported change in weight in kg. 
     */
    val data = records.map { r => (r("weight2"), r("wtyrago")) }.filterNot { case (w, w2) => w.isNaN || w2.isNaN }
    val changes = data.map { case (w, w2) => Number(w - w2) }
    println("Mean change " + mean(changes.toList))
  }

  def makeNormalModel(weights: Array[Double], xmax: Double = 175.0, xlabel: String = "adult weight (kg)") {
    val cdf = Cdf.fromList(weights.toList map { Number(_) })

    val (mu, vari) = trimmedMeanVar(weights.sorted.toList map { Number(_) })
    println("n, Mean, Var: " + weights.size, mu, vari)

    val sigma = math.sqrt(vari)
    println("Sigma: " + sigma)

    linePlot2(continuous.renderNormalCdf(mu, sigma, xmax), "weight model", cdf.render, "weight data", xlabel)

  }

  //4.11
  def makeFigures {
    /*
     * Generates CDFs and normal prob plots for weights and log weights.
     */

    val weights = records.map(_("wtkg2")).filterNot(_.isNaN)
    makeNormalModel(weights)
    continuous.normalPlot(weights.toList map { Number(_) }, "Adult weight")

    val logWeights = weights.map(math.log)
    makeNormalModel(logWeights, xmax = math.log(175.0), xlabel = "adult weight (log kg)")
    continuous.normalPlot(logWeights.toList map { Number(_) }, title = "Adult weight log")
  }

}

object brfss {
  def main(args: Array[String]) {
    val resp = new BRFSSRespondents
    resp.readRecords
    println("Height (cm):")
    resp.summarizeParam("htm3")
    println("Weight (kg):")
    resp.summarizeParam("weight2")
    resp.summarizeWeightChange
    resp.makeFigures
  }

}