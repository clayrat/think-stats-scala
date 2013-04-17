package ugent

import spire.math._
import spire.math.compat._

class BRFSSRespondents extends Table {
  val filename = "data/BRFSlight.ASC.gz"
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

  def summary(t: List[Double]) {
    val (mu, vari) = thinkstats.trimmedMeanVar(t map { Number(_) })
    val sigma = math.sqrt(vari)
    val cv = sigma / mu
    List(t.size, mu, vari, sigma, cv) map {d => print(d + " ")} 
    println
  }

  def summarizeParam(param: String) {
    /* 
     * Print summary statistics for male and female given parameter 
     */

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

  def summarizeWeightChange() {
    /* 
     * Print the mean reported change in weight in kg. 
     */

    val data = records.map { r => (r("weight2"), r("wtyrago")) }.filterNot { case (w, w2) => w.isNaN || w2.isNaN }
    val changes = data.map { case (w, w2) => Number(w - w2) }
    print("Mean change " + thinkstats.mean(changes.toList))
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
    resp.summarizeWeightChange()
  }

}