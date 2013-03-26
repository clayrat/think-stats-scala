package ugent

import scala.collection._

object first {

  def map2[A, B](abs: (List[A], List[A]), f: A => B): (List[B], List[B]) = (abs._1 map f, abs._2 map f)

  def main(args: Array[String]) {
    val pumpkins = List(1, 1, 1, 3, 3, 591).map(_.toDouble)
    val pMu = thinkstats.mean(pumpkins)
    val pSig2 = thinkstats.vari(pumpkins)
    val pSig = thinkstats.stddev(pumpkins)
    println("Pumpkins' mu, sigma2 and sigma: " + pMu + " " + pSig2 + " " + pSig)

    val table = new Pregnancies
    table.readRecords
    println("Number of pregnancies: " + table.records.size)

    val liveBirths = table.records.filter(_("outcome") == 1)
    val (firstKid, nonfirstKid) = map2(liveBirths.partition(_("birthord") == 1), { a: mutable.Map[String, Double] => a("prglength") })

    println("Number of live births: " + liveBirths.size)
    println("Average pregnancy for first kid: " + thinkstats.mean(firstKid))
    println("Average pregnancy for non-first kid: " + thinkstats.mean(nonfirstKid))
    println("Pregnancy sigma for first kid: " + thinkstats.stddev(firstKid))
    println("Pregnancy sigma for non-first kid: " + thinkstats.stddev(nonfirstKid))

  }
}