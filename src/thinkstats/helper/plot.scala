package thinkstats.helper

import spire.math._
import spire.math.compat._
import javax.swing.JFrame
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.axis.LogarithmicAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYBarRenderer
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection;

object plot {

  def render(chart: org.jfree.chart.JFreeChart, title: String) {
    val frame = new JFrame(title)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(640, 420)
    frame.add(new ChartPanel(chart))
    frame.pack()
    frame.setVisible(true)
  }

  def histPlot(data: List[(Number, Number)], title: String, xtitle: String = "X", ytitle: String = "Y") {
    val series = new XYSeries("")
    data map { case (x, y) => series.add(x, y) }
    val x = data.unzip._1
    val dataset = new XYSeriesCollection(series)

    val chart = ChartFactory.createXYBarChart(
      title, // chart title
      xtitle, // domain axis label
      false, // X axis shows dates?
      ytitle, // range axis label
      dataset, // data
      PlotOrientation.VERTICAL,
      false, // include legend?
      true, // tooltips?
      false) // URLs?

    val plot = chart.getPlot().asInstanceOf[XYPlot]
    val renderer = plot.getRenderer().asInstanceOf[XYBarRenderer]
    renderer.setMargin(0.98)
    renderer.setDrawBarOutline(true)

    render(chart, title)
  }

  def linePlot(data: List[(Number, Number)], title: String, xtitle: String = "X", ytitle: String = "Y", logX: Boolean = false, logY: Boolean = false) {
    val series = new XYSeries("")
    data map { case (x, y) => series.add(x, y) }
    val dataset = new XYSeriesCollection(series)

    val chart = ChartFactory.createXYLineChart(
      title, // chart title
      xtitle, // domain axis label
      ytitle, // range axis label
      dataset, // data
      PlotOrientation.VERTICAL,
      false, // include legend?
      true, // tooltips?
      false) // URLs?

    val plot = chart.getPlot().asInstanceOf[XYPlot]
    if (logX) {
      val domainAxis = new LogarithmicAxis("log(" + xtitle + ")");
      plot.setDomainAxis(domainAxis);
    }
    if (logY) {
      val rangeAxis = new LogarithmicAxis("log(" + ytitle + ")");
      plot.setRangeAxis(rangeAxis);
    }

    render(chart, title)
  }

  def linePlot2(data: List[(Number, Number)], title: String, data2: List[(Number, Number)], title2: String, xtitle: String = "X", ytitle: String = "Y") {
    val series1 = new XYSeries(title)
    data map { case (x, y) => series1.add(x, y) }
    val series2 = new XYSeries(title2)
    data2 map { case (x, y) => series2.add(x, y) }

    val dataset = new XYSeriesCollection()
    dataset.addSeries(series1)
    dataset.addSeries(series2)

    val chart = ChartFactory.createXYLineChart(
      title + " and " + title2,
      xtitle,
      ytitle,
      dataset,
      PlotOrientation.VERTICAL,
      true, // include legend?
      true,
      false)

    render(chart, title + " and " + title2)
  }

  def scatterPlot(data: List[(Number, Number)], title: String, xtitle: String = "X", ytitle: String = "Y") {
    val series = new XYSeries("")
    data map { case (x, y) => series.add(x, y) }
    val dataset = new XYSeriesCollection(series)

    val chart = ChartFactory.createScatterPlot(
      title, // chart title
      xtitle, // domain axis label
      ytitle, // range axis label
      dataset, // data
      PlotOrientation.VERTICAL,
      false, // include legend?
      true, // tooltips?
      false) // URLs?

    render(chart, title)
  }

}