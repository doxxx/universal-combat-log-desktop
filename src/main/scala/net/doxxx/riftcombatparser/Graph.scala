package net.doxxx.riftcombatparser

import swing.Component
import java.awt._

class Graph extends Component {
  private val PAD = 10
  private val POINT_SIZE = 2

  private var _data: Array[Int] = Array.empty

  preferredSize = new Dimension(200, 200)

  def data: Array[Int] = _data
  def data_=(newData: Array[Int]) {
    _data = newData
    repaint()
  }

  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)
    val w = size.width
    val h = size.height

    val maxVal = (math.ceil(data.max.toDouble / 1000d) * 1000).toInt
    val maxValW = g.getFontMetrics.stringWidth(maxVal.toString)
    val lPad = PAD + maxValW
    val rPad = PAD
    val tPad = PAD
    val bPad = PAD

    // paint background
    g.setPaint(Color.white)
    g.fillRect(0, 0, w, h)

    // paint axes
    g.setPaint(Color.black)
    g.drawLine(lPad, tPad, lPad, h - bPad) // Y-axis
    g.drawLine(lPad, h - bPad, w - rPad, h - bPad) // X-axis

    // paint scale on axes
    g.drawString(maxVal.toString, lPad - maxValW - 5, tPad + 5)
    // TODO: draw more Y-axis values?
    drawPoints(g, lPad, tPad, lPad, h - bPad, maxVal / 1000)
    drawPoints(g, lPad, h - bPad, w - rPad, h - bPad, data.size - 1)

    // TODO: draw horizontal grid in grey?

    if (data.length == 0) return

    // scale data to component size
    val xScale = (w - lPad - rPad).toDouble / (data.length.toDouble - 1)
    val yScale = (h - tPad - bPad).toDouble / (maxVal.toDouble - 1)

    // calculate origin
    val x0 = lPad
    val y0 = h - tPad

    // paint data points
    g.setPaint(Color.red)
    var j = 0
    while (j < data.length) {
      val x = x0 + (xScale * j).toInt
      val y = y0 - (yScale * data(j)).toInt
      drawPoint(g, x, y)
      if (j > 0) {
        val px = x0 + (xScale * (j - 1)).toInt
        val py = y0 - (yScale * data(j - 1)).toInt
        g.drawLine(px, py, x, y)
      }
      j += 1
    }
  }

  private def drawPoint(g: Graphics2D, x: Int, y: Int) {
    g.fillOval(x - POINT_SIZE, y - POINT_SIZE, POINT_SIZE*2, POINT_SIZE*2)
  }

  private def drawPoints(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int, count: Int) {
    var i = 0
    while (i < count) {
      drawPoint(g, x1 + (x2 - x1) * i / count, y1 + (y2 - y1) * i / count)
      i += 1
    }
  }

}

