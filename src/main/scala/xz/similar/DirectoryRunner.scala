package xz.similar

import java.io.File

import scala.collection.convert.Wrappers.MutableMapWrapper
import scala.collection.mutable

object DirectoryRunner {

  val threads = 4
  val targetSide = 64

  val logging = true

  def printProgress(dot: Int, of: Int) = () //print(s"\r[${"*" * dot}${"-" * (of - dot)}]")

  case class ImgFile(file: File, image: Img) {
    def this(file: File) = this(file, pixelate(file))
  }

  def main(args: Array[String]): Unit = {

    if (args.length < 1) throw new IllegalArgumentException("No directory provided")

    val dir = args(0)

    println("Browsing directory: " + dir)
    var images = getListOfFiles(dir, List(".jpg"))

    if (images.isEmpty) println("No images in directory.")

    val noOfImages = images.length
    var counter = 0
    printProgress(counter, noOfImages)

    var prev = new ImgFile(images.head)
    counter += 1
    printProgress(counter, noOfImages)
    images = images.tail
    val similarPairs = mutable.Map[String, Map[String, Int]]()
    val differentPairs = mutable.Map[String, Map[String, Int]]()
    while (images.nonEmpty) {
      val cur = new ImgFile(images.head)
      images = images.tail

      val namesPair: (String, String) = (prev.file.getName, cur.file.getName)
      val similarByName = namesPair._1.split('.')(0).last == namesPair._2.split('.')(0).last

      if (sameDimensions(prev, cur)) {
//        println(if (similarByName) s"Files ${prevFile.getName} and ${curFile.getName} are SIMILAR ")
        print(s"Files ${prev.file.getName} and ${cur.file.getName} are ${if (similarByName) "SIMILAR" else "DIFFERENT"}\t")
        val strategies = strategiesByName.keys.map(strat => (strat, strategiesByName(strat)(prev, cur).get)).toMap
        strategies.foreach{ case (name, value) => print(s"$name($value)\t")}
        if (similarByName) {
          similarPairs.put(namesPair._1 + namesPair._2, strategies)
        } else {
          differentPairs.put(namesPair._1 + namesPair._2, strategies)
        }
      } else {
        print(s"Files ${prev.file.getName} and ${cur.file.getName} have different dimensions ")
      }

      println()
      counter += 1
      printProgress(counter, noOfImages)
      prev = cur
    }

    val resultsSimilar = strategiesByName.keys.map(strat => (strat, similarPairs.mapValues(_(strat)).values.toList.sorted.reverse)).toMap
    val resultsDifferent = strategiesByName.keys.map(strat => (strat, differentPairs.mapValues(_(strat)).values.toList.sorted)).toMap
    println("Results for SIMILAR:")
    strategiesByName.keys.foreach(strat => println(s"$strat: ${resultsSimilar(strat)}"))
    println("Results for DIFFERENT:")
    strategiesByName.keys.foreach(strat => println(s"$strat: ${resultsDifferent(strat)}"))

    println("Results ranges:")
    strategiesByName.keys.foreach(strat => println(s"$strat:\tMAX SIMILAR(${resultsSimilar(strat).max})\tMIN DIFFERENT(${resultsDifferent(strat).min})"))
  }

  val strategiesByName = Map(
    "Total" -> diffTotal,
    "ByChannels" -> diffMax,
    "TotalSq" -> diffSqTotal,
    "ByChannelsSq" -> diffSqMax,
    "Sizes" -> diffSize,
    "TimeTaken" -> diffTimeTaken
  )

  def pixelate(prevFile: File): Img = {
    Pixelate.applyFilter(loadFileImage(prevFile), threads, targetSide)
//    time {  }
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

  type ImageDiffStrategy = (Img, Img) => IndexedSeq[Quad]

  def absDiffs(one: Img, two: Img): IndexedSeq[Quad] = {
    for {
      x <- 0 until one.width
      y <- 0 until one.height
    } yield Quad.absDiff(Quad(one(x, y)), Quad(two(x, y)))
  }

  def divergences(one: Img, two: Img): IndexedSeq[Quad] = {
    for {
      x <- 0 until one.width
      y <- 0 until one.height
    } yield Quad.divergence(Quad(one(x, y)), Quad(two(x, y)))
  }

  type ImageDiffCalculator = (Img, Img, ImageDiffStrategy) => Int

  def total(one: Img, two: Img, oper: ImageDiffStrategy): Int = {
    oper(one, two).foldLeft(0)(_ + Quad.sum(_)) / (one.height * one.width)
  }

  def maxOfChannels(one: Img, two: Img, oper: ImageDiffStrategy): Int = {
    Quad.toList(Quad.div(oper(one, two).foldLeft(Quad.zero)(Quad.add), one.height * one.width)).max
  }

  type ImgDiffMetric = (ImgFile, ImgFile) => Option[Int]

  def imagesDiffMetric(pixelDiff: ImageDiffStrategy, imageDiff: ImageDiffCalculator)(one: ImgFile, two: ImgFile): Option[Int] = {
    if (!sameDimensions(one, two))
      None

    Some(imageDiff(one.image, two.image, pixelDiff))
  }

  def diffSqMax: ImgDiffMetric = imagesDiffMetric(divergences, maxOfChannels)
  def diffSqTotal: ImgDiffMetric = imagesDiffMetric(divergences, total)
  def diffMax: ImgDiffMetric = imagesDiffMetric(absDiffs, maxOfChannels)
  def diffTotal: ImgDiffMetric = imagesDiffMetric(absDiffs, total)

  def diffTimeTaken: ImgDiffMetric = (one, two) => Some(Math.abs(one.file.lastModified() - two.file.lastModified()).toInt / 1000)
  def diffSize: ImgDiffMetric = (one, two) => Some(Math.abs(one.file.length() - two.file.length()).toInt)

  type SimilarStrategy = (ImgFile, ImgFile) => Boolean

  def sameDimensions: SimilarStrategy = (one, two) => one.image.height == two.image.height && one.image.width == two.image.width
  def nameLastChar: SimilarStrategy = (one, two) => one.file.getName.split('.')(0).last == two.file.getName.split('.')(0).last

  def strategyByMetric(strategy: ImgDiffMetric)(threshold: Int): SimilarStrategy = (img1, img2) => {
    strategy(img1, img2) match {
      case Some(sum) => sum <= threshold
      case None => false
    }
  }

  def areImagesSimilar(img1: ImgFile, img2: ImgFile, strategy: SimilarStrategy): Boolean = strategy(img1, img2)

}
