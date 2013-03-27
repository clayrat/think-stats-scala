package ugent

import scala.collection._

object first {

  def liveFirstNonFirst(table: Table): (List[Double], List[Double], List[Double]) = {
    val liveBirths = table.records.filter(_("outcome") == 1)
    val (firstKid, nonfirstKid) = liveBirths.partition(_("birthord") == 1)
    util.map3((liveBirths, firstKid, nonfirstKid), { a: mutable.Map[String, Double] => a("prglength") })
  }

  def main(args: Array[String]) {
    val pumpkins = List(1, 1, 1, 3, 3, 591).map(_.toDouble)
    val pMu = thinkstats.mean(pumpkins)
    val pSig2 = thinkstats.variance(pumpkins)
    val pSig = thinkstats.stddev(pumpkins)
    println("Pumpkins' mu, sigma2 and sigma: " + pMu + " " + pSig2 + " " + pSig)

    val table = new Pregnancies
    table.readRecords
    println("Number of pregnancies: " + table.records.size)

    val (live, first, nonfirst) = liveFirstNonFirst(table)

    println("Number of live births: " + live.size)
    println("Average pregnancy for first kid: " + thinkstats.mean(first))
    println("Average pregnancy for non-first kid: " + thinkstats.mean(nonfirst))
    println("Pregnancy sigma for first kid: " + thinkstats.stddev(first))
    println("Pregnancy sigma for non-first kid: " + thinkstats.stddev(nonfirst))

  }
}