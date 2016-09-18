package xz.similar

import xz.common._
import Math.{min, max}


/** A simple, trivially parallelizable computation. */
object Pixelate {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def pixelate(src: Img, dst: Img, from: Int, end: Int, square: Int, offsetx: Int, offsety: Int): Unit = {
    for {
      y <- from until min(end, dst.height)
      x <- 0 until dst.width
    } dst.update(x, y, pixelMean(src, x, y, square, offsetx, offsety))

  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parPixelate(src: Img, dst: Img, numTasks: Int, side: Int, offsetx: Int, offsety: Int): Unit = {
    val lines = dst.height
    val linesPerTask = lines / numTasks
    val ranges = (0 until lines).by(linesPerTask).map((y) => (y, y + linesPerTask))
    val tasks = ranges.map { case (from, until) => task(pixelate(src, dst, from, until, side, offsetx, offsety)) }
    tasks.foreach(_.join())
  }

  def applyFilter(image: Img, numTasks: Int, pixels: Int): Img = {
    val (w, h) = (image.width, image.height)
    val aspect: Double = min(w, h) / Int.int2double(max(w, h))
    val (offsetx, offsety) = (w % pixels / 2, h % pixels / 2)
    val pixelSide = max(w, h) / pixels
    val shortSide: Int = min(w, h) / pixelSide
    val dst = if (w > h) new Img(pixels, shortSide) else new Img(shortSide, pixels)
    numTasks match {
      case 1 => pixelate(image, dst, 0, dst.height, pixelSide, offsetx, offsety)
      case _ => parPixelate(image, dst, numTasks, pixelSide, offsetx, offsety)
    }
    dst
  }

}
