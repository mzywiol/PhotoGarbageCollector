package xz.similar

import java.io.File

object DirectoryRunner {

  val threads = 4
  val targetSide = 64

  val logging = true

  def main(args: Array[String]): Unit = {

    if (args.length < 1) throw new IllegalArgumentException("No directory provided")

    val dir = args(0)

    println("Browsing directory: " + dir)
    var images = getListOfFiles(dir, List(".jpg"))

    if (images.isEmpty) println("No images in directory.")

    var prevFile = images.head
    var prevMean: Img = pixelate(prevFile)
    images = images.tail
    while (images.nonEmpty) {
      val curFile = images.head
      images = images.tail
      val curMean: Img = pixelate(curFile)

      print(s"Files ${prevFile.getName} and ${curFile.getName} : ")
      print(if (prevFile.getName.split('.')(0).last == curFile.getName.split('.')(0).last) " SIMILAR " else "         ")
      print(s"  diffTotal(${diffImages(prevMean, curMean)})")
      print(s"  diffByChannels(${diffByChannels(prevMean, curMean)})")
      println("")

      prevFile = curFile
      prevMean = curMean
    }
  }

  def pixelate(prevFile: File): Img = {
    time { Pixelate.applyFilter(loadFileImage(prevFile), threads, targetSide) }
  }

  def getListOfFiles(dir: String, suffixes: List[String]): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).filter {
        (file) => suffixes.contains(file.getName.substring(file.getName.lastIndexOf('.')).toLowerCase)
      }.toList
    } else {
      List[File]()
    }
  }

  type SimilarStrategy = (Img, Img) => Boolean

  def sameDimensions: SimilarStrategy = (one, two) => one.height == two.height && one.width == two.width

  def areImagesSimilar(img1: Img, img2: Img, strategy: SimilarStrategy): Boolean = strategy(img1, img2)

  def strategyTotal(threshold: Int): SimilarStrategy = (img1, img2) => {
    diffImages(img1, img2) match {
      case Some(sum) => sum <= threshold
      case None => false
    }
  }

  def strategyByChannel(threshold: Int): SimilarStrategy = (img1, img2) => {
    diffByChannels(img1, img2) match {
      case Some(ch) => ch <= threshold
      case None => false
    }
  }

  def diffImages(one: Img, two: Img): Option[Int] = {
    if (!sameDimensions(one, two))
      None

    val pixels = one.height * one.width
    Some((for {
      x <- 0 until one.width
      y <- 0 until one.height
    } yield Quad.absDiff(Quad(one(x, y)), Quad(two(x, y)))).foldLeft(0)(_ + Quad.sum(_)) / pixels)
  }

  def diffByChannels(one: Img, two: Img): Option[Int] = {
    if (!sameDimensions(one, two))
      None

    val pixels = one.height * one.width
    Some(Quad.toList(Quad.div((for {
      x <- 0 until one.width
      y <- 0 until one.height
    } yield Quad.absDiff(Quad(one(x, y)), Quad(two(x, y)))).foldLeft(Quad.zero)(Quad.add), pixels)).max)
  }
}
