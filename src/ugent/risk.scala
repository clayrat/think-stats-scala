package ugent

object risk {

  def probRange(pmf: Pmf[Int], a: Int, b: Int): Double = {
    pmf.items.filter(x => (x._1 >= a && x._1 <= b)).unzip._2.sum
  }
  def probEarly(pmf: Pmf[Int]) = probRange(pmf, pmf.values.min, 37)
  def probOnTime(pmf: Pmf[Int]) = probRange(pmf, 38, 40)
  def probLate(pmf: Pmf[Int]) = probRange(pmf, 41, pmf.values.max)

  def main(args: Array[String]) {

    val table = new Pregnancies
    table.readRecords
    val (live, firstK, nonfirstK) = util.map3(first.liveFirstNonFirst(table), { a: Double => a.toInt })

    val pmfLive = new Pmf[Int](live)
    val pmfFirst = new Pmf[Int](firstK)
    val pmfNonfirst = new Pmf[Int](nonfirstK)
    
    println("Probabilities for first babies: " + probEarly(pmfFirst) + " " + probOnTime(pmfFirst) + " " + probLate(pmfFirst))
    println("Probabilities for non-first babies: " + probEarly(pmfNonfirst) + " " + probOnTime(pmfNonfirst) + " " + probLate(pmfNonfirst))
    println("Probabilities for all babies: " + probEarly(pmfLive) + " " + probOnTime(pmfLive) + " " + probLate(pmfLive))
    println("Relative risk: " + probEarly(pmfFirst)/probEarly(pmfNonfirst))

  }

}