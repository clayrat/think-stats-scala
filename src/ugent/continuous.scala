package ugent

import org.apache.commons.math3.distribution._
import spire.math._
import spire.math.compat._

object continuous {

  def removeZero(a: List[(Number, Number)]) = a.dropRight(1) :+ (a.last._1, a.dropRight(1).last._2)

  def main(args: Array[String]) {

    val babyMinutes = List(5, 64, 78, 115, 177, 245, 247, 262, 271, 428, 455, 492, 494, 549, 635, 649, 653, 693, 729, 776, 785, 846, 847, 873, 886, 914, 991, 1017, 1062, 1087, 1105, 1134, 1149, 1187, 1189, 1191, 1210, 1237, 1251, 1264, 1283, 1337, 1407, 1435)
    val intertimes = (babyMinutes.drop(1) zip babyMinutes.dropRight(1)) map { case (a, b) => a - b }
    val babyCdf = Cdf.fromList(intertimes map { Number(_) })
    plot.linePlot(babyCdf.render, "interrival times CDF")
    plot.linePlot(removeZero(babyCdf.renderCCDF), "interrival times CCDF log", logScale = true)

    // 4.1
    val expdist = new ExponentialDistribution(32.6)
    val denslist = List.fill(44)(expdist.sample)
    val cdf = Cdf.fromList(denslist map { Number(_) })
    plot.linePlot(removeZero(cdf.renderCCDF), "exponential samples CCDF log", logScale = true)
  }

}