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

  def binomialPMF(n: Int, p: Double)(k: Int): Double =
    binomCoef(n, k) * math.pow(p, k) * math.pow(1 - p, n - k)

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

    def randomSetElement[A](s: Set[A]) = s.toList(Random.nextInt(s.size))

    def montyRun(switch: Boolean): Boolean = {
      val doors = Set('A', 'B', 'C')
      val prize = randomSetElement(doors)
      val choice = randomSetElement(doors)
      val opens = if (choice == prize) randomSetElement(doors - choice) else (doors - prize - choice).head
      prize == (if (switch) (doors - opens - choice).head else choice)
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

  // 5.10
  def hundredCoins =
    println("Probability of getting exactly 50 heads: " + binomialPMF(100, 0.5)(50))

  // 5.11
  def basketballMC(runs: Int) {

    // (f, f, t, t, t, f) => (2, 3, 1)
    def packSizes[A](ls: List[A]): List[Int] = {
      if (ls.isEmpty) List()
      else {
        val (packed, next) = ls span { _ == ls.head }
        if (next == Nil) List(packed.size)
        else packed.size +: packSizes(next)
      }
    }

    def tenOrMore(ls: List[Int]) = ls.map(_ >= 10).reduce(_ || _)

    def game = List.fill(10)(List.fill(15)(Random.nextBoolean))
    def games(n: Int) = List.fill(n)(game.map { p => tenOrMore(packSizes(p)) }.reduce(_ || _))
    def seasons(n: Int) = List.fill(n)(games(82).reduce(_ || _))

    def ratio(ls: List[Boolean]) = ls.filter(a => a).size.toDouble / ls.size

    println("Probability of a 10-streak in a game: " + ratio(games(runs)))
    println("Probability of a 10-streak in a season: " + ratio(seasons(runs)))
  }

  // 5.12 
  // baseball rules are a mystery to me

  // 5.13
  def cancerCluster {
    def cancer: Boolean = Random.nextInt(1000) == 999

    def cohort(years: Int) = {
      def newCohort = Array.fill(100)(cancer)
      def cohortHelper(c: Array[Boolean], year: Int): Array[Boolean] = {
        if (year < years) cohortHelper(c map {
          _ match {
            case true => true
            case false => cancer
          }
        }, year + 1)
        else c
      }
      cohortHelper(newCohort, 0)
    }

    // 1)
    val runs1 = 1000

    val cohortCancers = List.fill(runs1)(cohort(10).count(a => a)) map { Number(_) }
    val cohortPmf = Pmf.fromList(cohortCancers)
    histPlot(renderPmf(cohortPmf), "Cancer cases in cohort PMF")

    // 2)
    def statCases(pmf: Pmf[Number], pval: Double): Int =
      pmf.items.sorted.find { case (n, p) => p < pval }.get._1.toInt

    val statp005 = statCases(cohortPmf, 0.05)
    println("Statistically significant # of cases: " + statp005)

    // 3)
    val runs3 = 1000

    def tenThousand(cases: Int) = Array.fill(100)(cohort(10).count(a => a)).map(_ >= cases).reduce(_ || _)
    def trial(cases: Int) = Array.fill(runs3)(tenThousand(cases))

    val trial005 = trial(statp005)
    println("Chances for 10000 people are (p=0.05): " + trial005.count(a => a).toDouble / trial005.size)
    val statp001 = statCases(cohortPmf, 0.01)
    val trial001 = trial(statp001)
    println("Chances for 10000 people are (p=0.01): " + trial001.count(a => a).toDouble / trial001.size)

    // 4)
    val runs4 = 1000

    def block10x10(g: Array[Array[Boolean]], x: Int, y: Int) =
      g.slice(y, y + 10).map(_.slice(x, x + 10).count(a => a)).sum

    def gridTrial(runs: Int, cases: Int): Double = {
      val trials = for (n <- 1 to runs) yield {
        val grid = Array.fill(100)(cohort(10))
        lazy val blocks = for (x <- 0 to 90; y <- 0 to 90) yield (block10x10(grid, x, y) >= cases)
        blocks.reduce(_ || _)
      }
      trials.count(a => a).toDouble / trials.size
    }
    println("Chances for 100x100 grid are (p=0.05): " + gridTrial(runs4, statp005))
    println("Chances for 100x100 grid are (p=0.01): " + gridTrial(runs4, statp001))

    // 5)
    // meh, 10 years already give ~1.0
  }

  def main(args: Array[String]) {
    /*oneIsSix
    hundredDice
    montyHall(1000)
    baker
    dancePairs(1000)
    hundredCoins
    basketballMC(1000)*/
    cancerCluster
  }

}