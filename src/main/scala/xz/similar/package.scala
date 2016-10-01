package xz

import java.io.{File, FileInputStream, InputStream}
import javax.imageio.ImageIO

package object similar {

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    print(" || time: " + (System.nanoTime - s) / 1e6 + "ms || ")
    ret
  }

  /** The value of every pixel is represented as a 32 bit integer. */
  type ARGB = Int

  /** Returns the red component. */
  def red(c: ARGB): Int = (0xff0000 & c) >>> 16

  /** Returns the green component. */
  def green(c: ARGB): Int = (0xff00 & c) >>> 8

  /** Returns the blue component. */
  def blue(c: ARGB): Int = 0xff & c

  /** Returns the alpha component. */
  def alpha(c: ARGB): Int = (0xff000000 & c) >>> 24

  object ARGB {
    def apply(a: Int, r: Int, g: Int, b: Int): ARGB = {
      ((a & 0xff) << 24) | ((r & 0xff) << 16) | ((g & 0xff) << 8) | (b & 0xff)
    }

    def apply(quad: Quad): ARGB = {
      apply(quad._1, quad._2, quad._3, quad._4)
    }
  }

  type Quad = (Int, Int, Int, Int)

  object Quad {

    def apply(argb: ARGB) = (alpha(argb), red(argb), green(argb), blue(argb))

    val zero = (0, 0, 0, 0)

    private def quadOperation(o: (Int, Int) => Int)(a: Quad, b: Quad): Quad =
      (o(a._1, b._1), o(a._2, b._2), o(a._3, b._3), o(a._4, b._4))

    def add(a: Quad, b: Quad): Quad = quadOperation((x, y) => x + y)(a, b)
    def absDiff(a: Quad, b: Quad): Quad = quadOperation((x, y) => Math.abs(x - y))(a, b)

    def div(a: Quad, by: Int): Quad = (a._1 / by, a._2 / by, a._3 / by, a._4 / by)

    def sum(a: Quad) = a._1 + a._2 + a._3 + a._4

    def toList(a: Quad): Iterable[Int] = Vector(a._1, a._2, a._3, a._4)
  }

  /** Restricts the integer into the specified range, end exclusive. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v >= max) max - 1
    else v
  }

  def divided(side: Int, by: Int): Int =
    (side / by) + (if (side % by == 0) 0 else 1)

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[ARGB]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): ARGB = data(y * width + x)
    def update(x: Int, y: Int, c: ARGB): Unit = data(y * width + x) = c
    def pixels = data.length
  }

  def mean(src: Img, minx: Int, maxx: Int, miny: Int, maxy: Int): ARGB = {
    val pixels = (maxx - minx + 1) * (maxy - miny + 1)
    var (ca, cr, cg, cb) = (0, 0, 0, 0)
    var row = miny
    while(row <= maxy) {
      var col = minx
      while(col <= maxx) {
        val pixel = src(col, row)
        ca += alpha(pixel)
        cr += red(pixel)
        cg += green(pixel)
        cb += blue(pixel)
        col += 1
      }
      row += 1
    }
    ARGB(ca / pixels, cr / pixels, cg / pixels, cb / pixels)
  }

  /**
    * Count mean pixel value for given square of given image.
    *
    * @param src given image
    * @param x column index of the square
    * @param y row index of the square
    * @param side pixel length of quare side
    * @param offsetx horizontal offset for the start of first column of squares
    * @param offsety vertical offset for the start of first row of squares
    * @return mean pixel value of all pixels in the square of (x,y) coorditates
    */
  def pixelMean(src: Img, x: Int, y: Int, side: Int, offsetx: Int = 0, offsety: Int = 0): ARGB = {
    val (minx, miny) = (offsetx + x * side, offsety + y * side)
    val (maxx, maxy) = (clamp(minx + side, 0, src.width), clamp(miny + side, 0, src.height))
    mean(src, minx, maxx, miny, maxy)
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlur(src: Img, x: Int, y: Int, radius: Int): ARGB = {
    val (minx, maxx) = (clamp(x - radius, 0, src.width),  clamp(x + radius, 0, src.width))
    val (miny, maxy) = (clamp(y - radius, 0, src.height), clamp(y + radius, 0, src.height))
    mean(src, minx, maxx, miny, maxy)
  }

  // file operations

  private def loadFileImage(stream: FileInputStream): Img = {
    try {
      loadImage(stream)
    } finally {
      stream.close()
    }
  }

  def loadFileImage(path: String): Img = loadFileImage(new FileInputStream(path))

  def loadFileImage(file: File): Img = loadFileImage(new FileInputStream(file))

  def loadImage(inputStream: InputStream): Img = {
    val bufferedImage = ImageIO.read(inputStream)
    val width = bufferedImage.getWidth
    val height = bufferedImage.getHeight
    val img = new Img(width, height)
    for (x <- 0 until width; y <- 0 until height) img(x, y) = bufferedImage.getRGB(x, y)
    img
  }

}
