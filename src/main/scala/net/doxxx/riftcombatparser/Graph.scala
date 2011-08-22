package net.doxxx.riftcombatparser

import swing.Component
import java.awt._

class Graph extends Component {
  private val PAD = 20

  private var _data: Array[Int] = Array.empty

  preferredSize = new Dimension(200,200)

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

    // paint background
    g.setPaint(Color.white)
    g.fillRect(0, 0, w, h)

    // paint axes
    g.setPaint(Color.black)
    g.drawLine(PAD, PAD, PAD, h - PAD)
    g.drawLine(PAD, h - PAD, w - PAD, h - PAD)

    if (data.length == 0) return

    // scale data to component size
    val xScale = (w - 2 * PAD).toDouble / (data.length.toDouble - 1)
    val max = data.max
    val yScale = (h - 2 * PAD).toDouble / (max.toDouble - 1)

    // calculate origin
    val x0 = PAD
    val y0 = h - PAD

    // paint data points
    g.setPaint(Color.red)
    var j = 0
    while (j < data.length) {
      val x = x0 + (xScale * j).toInt;
      val y = y0 - (yScale * data(j)).toInt;
      g.fillOval(x - 2, y - 2, 4, 4);
      if (j > 0) {
        val px = x0 + (xScale * (j - 1)).toInt;
        val py = y0 - (yScale * data(j - 1)).toInt;
        g.drawLine(px, py, x, y)
      }
      j += 1
    }
  }
}

