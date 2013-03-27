package ugent

object conditional {

  def conditionalPmf[A](pmf: Pmf[A], condition: A => Boolean): Pmf[A] = {
    val newPmf = new Pmf[A](pmf.items.filterNot({ a => condition(a._1) }).toMap)
    newPmf.normalize
    newPmf
  }

  def conditionalBirthProb(pmf: Pmf[Int], week: Int): Double =
    conditionalPmf[Int](pmf, { a: Int => a < week }).prob(week)

  def main(args: Array[String]) {
    val table = new Pregnancies
    table.readRecords
    val (live, firstK, nonfirstK) = util.map3(first.liveFirstNonFirst(table), { a: Double => a.toInt })
    val firstPmf = new Pmf[Int](firstK)
    val nonfirstPmf = new Pmf[Int](nonfirstK)

    val firstProbs = for (x <- (35 to 46)) yield conditionalBirthProb(firstPmf, x)
    val nonfirstProbs = for (x <- (35 to 46)) yield conditionalBirthProb(nonfirstPmf, x)

    println(firstProbs)
    println(nonfirstProbs)

  }

}