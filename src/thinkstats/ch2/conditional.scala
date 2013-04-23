package thinkstats.ch2

import thinkstats.ch1._
import spire.math._
import spire.math.compat._
import thinkstats.helper.plot.linePlot2
import thinkstats.helper.util.map3

object conditional {

  //2.7
  def conditionalPmf[A](pmf: Pmf[A], condition: A => Boolean): Pmf[A] =
    new Pmf[A](pmf.pmfMap.filterNot { a => condition(a._1) }).normalized

  def conditionalBirthProb(pmf: Pmf[Int], week: Int): Double =
    conditionalPmf[Int](pmf, { a: Int => a < week }).prob(week)

  def main(args: Array[String]) {

    import thinkstats.helper.util._
    import thinkstats.helper.plot._

    val table = new Pregnancies
    table.readRecords
    val (live, firstK, nonfirstK) = map3(first.liveFirstNonFirst(table), { a: Double => a.toInt })
    val firstPmf = Pmf.fromList(firstK)
    val nonfirstPmf = Pmf.fromList(nonfirstK)

    val conditionalList = { pmf: Pmf[Int] => (35 to 46).map { week => (Number(week), Number(conditionalBirthProb(pmf, week))) }.toList }

    val firstProbs = conditionalList(firstPmf)
    val nonfirstProbs = conditionalList(nonfirstPmf)
    linePlot2(firstProbs, "first probs", nonfirstProbs, "non-first probs", xtitle = "week", ytitle = "P")
  }

}