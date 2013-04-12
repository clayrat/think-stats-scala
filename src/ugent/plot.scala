package ugent

import spire.math._
import spire.math.compat._

import javax.swing._
import org.math.plot._

object plot {

  def histPlot[T <: DictWrapper[Double, Double]](data: T, title: String) {
    val dat = data.items.unzip._1.toArray
    val plot = new Plot2DPanel
    val frame = new JFrame(title)
    frame.setSize(600, 600)
    frame.setContentPane(plot)
    frame.setVisible(true)
    plot.addHistogramPlot(title, dat, 100)
  }

  def linePlot(data: List[(Number,Number)], title: String) {
    val (x, y) = data.sorted.unzip
    val plot = new Plot2DPanel
    val frame = new JFrame(title)
    frame.setSize(600, 600)
    frame.setContentPane(plot)
    frame.setVisible(true)
    plot.addLinePlot(title, x.toArray.map(_.toDouble), y.toArray.map(_.toDouble))
  }
}