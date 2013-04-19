package ugent

import scala.util.Random
import jsc.distributions._

import spire.math._
import spire.math.compat._

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

  def baker {
    def heaviest(n: Int): Double = {
      val breadDist = new Normal(950, 50)
      List.fill(n)(breadDist.random).max
    }

    val breads = List.fill(365)(heaviest(4)) map { Number(_) }
    val breadMu = thinkstats.mean(breads)
    val breadSig = thinkstats.stddev(breads)
    val simBreadDist = new Normal(breadMu, breadSig)
    val simBreads = List.fill(365)(simBreadDist.random) map { Number(_) }

    val breadPmf = Pmf.fromList(numHist.binData(breads, 15))
    val breadCdf = Cdf.fromList(breads)
    val simuPmf = Pmf.fromList(numHist.binData(simBreads, 15))
    val simuCdf = Cdf.fromList(simBreads)

    plot.linePlot2(renderPmf(breadPmf), "Heaviest breads over a year", renderPmf(simuPmf), "Breads baker wants us to believe")
    plot.scatterPlot(breads.sorted zip simBreads.sorted, "Breads comparison")
    plot.linePlot2(breadCdf.render, "Heaviest breads over a year", simuCdf.render, "Breads baker wants us to believe")
  }

  def main(args: Array[String]) {
    oneIsSix
    hundredDice
    montyHall(1000)
    baker
  }

}