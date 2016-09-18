package xz.ui.similar

import java.awt._
import java.awt.image._
import javax.swing._
import Math.min

import xz.similar._

class PhotoCanvas extends JComponent {

  var imagePath: Option[String] = None

  var image = loadScalaImage()

  override def getPreferredSize = {
    new Dimension(image.width, image.height)
  }

  private def loadScalaImage(): Img = {
    val stream = this.getClass.getResourceAsStream("/scalashop/scala.jpg")
    try {
      loadImage(stream)
    } finally {
      stream.close()
    }
  }

  def reload(): Unit = {
    image = imagePath match {
      case Some(path) => loadFileImage(path)
      case None => loadScalaImage()
    }
    repaint()
  }

  def loadFile(path: String): Unit = {
    imagePath = Some(path)
    reload()
  }

  def applyFilter(numTasks: Int, targetSide: Int) {
    val horizontal = image.width > image.height
    val pixelSide = (if (horizontal) image.width else image.height) / targetSide
    val (ox, oy) = (image.width % targetSide / 2, image.height % targetSide / 2)
    val dst = Pixelate.applyFilter(image, numTasks, targetSide)
    for {
      x <- 0 until dst.width
      y <- 0 until dst.height
      dx <- (ox + x * pixelSide) until min(ox + (x+1) * pixelSide, image.width)
      dy <- (oy + y * pixelSide) until min(oy + (y+1) * pixelSide, image.height)
    } image.update(dx, dy, dst(x, y))

    repaint()
  }

  override def paintComponent(gcan: Graphics) = {
    super.paintComponent(gcan)

    val width = image.width
    val height = image.height
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for (x <- 0 until width; y <- 0 until height) bufferedImage.setRGB(x, y, image(x, y))

    gcan.drawImage(bufferedImage, 0, 0, null)
  }

}
