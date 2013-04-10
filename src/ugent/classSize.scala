package ugent

object classSize {

  implicit def pmf2intPmf(pmf: Pmf[Int]) = new intPmf(pmf)

  def biasedPmf(pmf: Pmf[Int]): Pmf[Int] = {
    val biased = pmf.copy
    biased.items map { a => biased.mult(a._1, a._1) }
    biased.normalize
    biased
  }

  def unbiasedPmf(pmf: Pmf[Int]): Pmf[Int] = {
    val unbiased = pmf.copy
    unbiased.items map { a => unbiased.mult(a._1, 1.0 / a._1) }
    unbiased.normalize
    unbiased
  }

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

    val classPmf = new Pmf[Int](new Hist[Int](classSize))
    val biased = biasedPmf(classPmf)
    val unbiased = unbiasedPmf(biased)

    println("Dean says " + classPmf.mean.round)
    println("Students say " + biased.mean.round)
    println("After unbiasing " + unbiased.mean.round)

  }

}