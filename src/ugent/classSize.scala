package ugent

object classSize {

  implicit def pmf2intPmf(pmf: Pmf[Int]) = intPmf.fromPmf(pmf)
  
  //3.1
  def biasedPmf(pmf: Pmf[Int]): Pmf[Int] =
    pmf.multiplied { a: Int => a }.normalized

  def unbiasedPmf(pmf: Pmf[Int]): Pmf[Int] =
    pmf.multiplied { a: Int => 1.0 / a }.normalized

  def main(args: Array[String]) {
    val classSize = Map(
      7 -> 8,
      12 -> 8,
      17 -> 14,
      22 -> 4,
      27 -> 6,
      32 -> 12,
      37 -> 8,
      42 -> 3,
      47 -> 2)

    val classPmf = Pmf.fromHist(Hist.fromMap(classSize))
    val biased = biasedPmf(classPmf)
    val unbiased = unbiasedPmf(biased)

    println("Dean says " + classPmf.mean.round)
    println("Students say " + biased.mean.round)
    println("After unbiasing " + unbiased.mean.round)

  }

}