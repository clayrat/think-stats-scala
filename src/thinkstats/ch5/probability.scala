package thinkstats.ch5

import thinkstats.helper.plot._
import thinkstats.helper.stats._
import thinkstats.ch2._
import thinkstats.ch3._

import scala.util.Random

import spire.math._
import spire.math.compat._

import jsc.distributions._

object probability {

  def renderPmf(pmf: Pmf[Number]): List[(Number, Number)] = pmf.items.map { case (x, p) => (x, Number(p)) }

  // 5.1
  def oneIsSix {
    val eights = for (
      a <- 1 to 6;
      b <- 1 to 6;
      if a + b == 8
    ) yield (a, b)
    val isSix = eights.filter { case (a, b) => a == 6 || b == 6 }
    println("chance of one dice being 6 is " + isSix.size.toDouble / eights.size)
  }

  // 5.2 
  def hundredDice {
    println("all 6s out of 100: " + math.pow(1.0 / 6, 100))
    println("no 6s out of 100: " + math.pow(5.0 / 6, 100))
  }

  // 5.3 
  // trivial: 1) 0.25, 2-4) 0.5

  // 5.4 + 5.5
  def montyHall(runs: Int) {

    def randomSetElement(s: Set[Char]) = s.toList(Random.nextInt(s.size))

    def montyRun(switch: Boolean): Boolean = {
      val doors = Set('A', 'B', 'C')
      val prize = randomSetElement(doors)
      val choice = randomSetElement(doors)
      val opens = if (choice == prize) randomSetElement(doors - choice) else (doors - prize - choice).head
      if (switch) (doors - opens - choice).head == prize else choice == prize
    }

    // Monty doesn't know the prize door
    def stupidMontyRun(switch: Boolean): Boolean = {
      val doors = Set('A', 'B', 'C')
      val prize = randomSetElement(doors)
      val choice = randomSetElement(doors)
      val opens = randomSetElement(doors - choice)
      if (prize == opens) false else choice == prize
    }

    val stickWins = List.fill(runs)(montyRun(false) compare false).sum
    val switchWins = List.fill(runs)(montyRun(true) compare false).sum
    println("Sticking wins: " + stickWins.toDouble / runs)
    println("Switching wins: " + switchWins.toDouble / runs)

    val stickWinsStupid = List.fill(runs)(stupidMontyRun(false) compare false).sum
    val switchWinsStupid = List.fill(runs)(stupidMontyRun(true) compare false).sum
    println("Stupid sticking wins: " + stickWinsStupid.toDouble / runs)
    println("Stupid switching wins: " + switchWinsStupid.toDouble / runs)
  }

  // 5.6
  def baker {
    def heaviest(n: Int): Double = {
      val breadDist = new Normal(950, 50)
      List.fill(n)(breadDist.random).max
    }

    // empirically, we establish that baker needs only 4 breads 
    val breads = List.fill(365)(heaviest(4)) map { Number(_) }
    val breadMu = mean(breads)
    val breadSig = stddev(breads)
    val simBreadDist = new Normal(breadMu, breadSig)
    val simBreads = List.fill(365)(simBreadDist.random) map { Number(_) }

    val breadPmf = Pmf.fromList(numHist.binData(breads, 15))
    val breadCdf = Cdf.fromList(breads)
    val simuPmf = Pmf.fromList(numHist.binData(simBreads, 15))
    val simuCdf = Cdf.fromList(simBreads)

    linePlot2(renderPmf(breadPmf), "Heaviest breads over a year", renderPmf(simuPmf), "Breads baker wants us to believe")
    scatterPlot(breads.sorted zip simBreads.sorted, "Breads comparison")
    linePlot2(breadCdf.render, "Heaviest breads over a year", simuCdf.render, "Breads baker wants us to believe")
  }

  // 5.7
  def dancePairs(n: Int) {
    val menDist = new Normal(178, math.sqrt(59.4))
    val womenDist = new Normal(163, math.sqrt(52.8))
    println("Coefficient of variation for men: " + menDist.sd / menDist.mean)
    println("Coefficient of variation for women: " + womenDist.sd / womenDist.mean)
    val pairsList = List.fill(n)(menDist.random) zip List.fill(n)(womenDist.random)
    println("Percentage of pairs with a higher woman: "
      + 100 * pairsList.filter { case (man, woman) => woman > man }.size.toDouble / pairsList.size)
  }

  // 5.8
  // 1/6 + 1/6 - 1/6^2 = 17/36 = 0.47(2)

  // 5.9
  // P(A xor B) = P(A) - P(B) - 2*P(A and B)

  def binomialPMF(n: Int, p: Double)(k: Int): Double =
    binomCoef(n, k) * math.pow(p, k) * math.pow(1 - p, n - k)

  // 5.10
  def hundredCoins {
    //println(binomCoef(100, 50))
    println("Probability of getting exactly 50 heads: " + binomialPMF(100, 0.5)(50))
  }

  def main(args: Array[String]) {
    /*oneIsSix
    hundredDice
    montyHall(1000)
    baker
    dancePairs(1000)*/
    hundredCoins
  }

}