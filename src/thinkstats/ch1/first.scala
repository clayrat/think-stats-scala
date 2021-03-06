package thinkstats.ch1

import thinkstats.helper.util._
import thinkstats.helper.stats._

import spire.math._
import spire.math.compat._

import scala.collection._

object first {

  def liveFirstNonFirst(table: Table): (List[Double], List[Double], List[Double]) = {
    val liveBirths = table.records.filter(_("outcome") == 1).toList
    val (firstKid, nonfirstKid) = liveBirths.partition(_("birthord") == 1)
    map3((liveBirths, firstKid, nonfirstKid), { a: mutable.Map[String, Double] => a("prglength") })
  }

  def main(args: Array[String]) {
    //1.3
    val table = new Pregnancies
    table.readRecords
    println("Number of pregnancies: " + table.records.size)
    val (live, first, nonfirst) = liveFirstNonFirst(table)
    println("Number of live births: " + live.size)
    println("Average pregnancy for first kid: " + mean(first map { Number(_) }))
    println("Average pregnancy for non-first kid: " + mean(nonfirst map { Number(_) }))

    //2.1
    val pumpkins = List(1, 1, 1, 3, 3, 591).map(_.toDouble)
    val pMu = mean(pumpkins map { Number(_) })
    val pSig2 = variance(pumpkins map { Number(_) })
    val pSig = stddev(pumpkins map { Number(_) })
    println("Pumpkins' mu, sigma2 and sigma: " + pMu + " " + pSig2 + " " + pSig)

    //2.2
    println("Pregnancy sigma for first kid: " + stddev(first map { Number(_) }))
    println("Pregnancy sigma for non-first kid: " + stddev(nonfirst map { Number(_) }))
  }
}