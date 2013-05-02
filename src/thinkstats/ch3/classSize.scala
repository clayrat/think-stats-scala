package thinkstats.ch3

import spire.math._
import spire.implicits._

import spire.syntax.si._

import thinkstats.ch2._
import thinkstats.ch2.numPmf._

object classSize {

 
  //3.1
  def biasedPmf(pmf: Pmf[Number]): Pmf[Number] =
    pmf.multiplied { a: Number => a.toDouble }.normalized

  def unbiasedPmf(pmf: Pmf[Number]): Pmf[Number] =
    pmf.multiplied { a: Number => 1.0 / a.toDouble }.normalized

  def main(args: Array[String]) {
    val classSize = Map(
      Number(7) -> j"8",
      Number(12) -> j"8",
      Number(17) -> j"14",
      Number(22) -> j"4",
      Number(27) -> j"6",
      Number(32) -> j"12",
      Number(37) -> j"8",
      Number(42) -> j"3",
      Number(47) -> j"2") 

    val classPmf = Pmf.fromHist(Hist.fromMap(classSize))
    val biased = biasedPmf(classPmf)
    val unbiased = unbiasedPmf(biased)

    println("Dean says " + classPmf.mean.round)
    println("Students say " + biased.mean.round)
    println("After unbiasing " + unbiased.mean.round)

  }

}