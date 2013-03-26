package ugent

object first {

  def mean(a: List[Double]): Double = a.sum / a.size

  def main(args: Array[String]) {
    val table = new Pregnancies
    table.readRecords
    println("Number of pregnancies: " + table.records.size)

    val liveBirths = table.records.filter(_("outcome") == 1)
    val (firstKid, nonfirstKid) = liveBirths.partition(_("birthord") == 1)

    println("Number of live births: " + liveBirths.size)
    println("Average pregnancy for first kid: " + mean(firstKid.map(_("prglength"))))
    println("Average pregnancy for non-first kid: " + mean(nonfirstKid.map(_("prglength"))))

  }
}