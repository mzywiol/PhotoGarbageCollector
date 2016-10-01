package xz.similar

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class ARGBTest extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  val anyint = Gen.choose(Int.MinValue, Int.MaxValue)
  val byte = Gen.choose(0, 255)
  val quadruple = for {
    a <- byte
    r <- byte
    g <- byte
    b <- byte
  } yield (a, r, g, b)

  "alpha returns value of first 8 bits only" in {
    forAll (byte, anyint) {
      (a, argb) => alpha(argb & 0x00ffffff | (a << 24)) shouldEqual a
    }
  }

  "red returns value of second 8 bits only" in {
    forAll (byte, anyint) {
      (r, argb) => red(argb & 0xff00ffff | (r << 16)) shouldEqual r
    }
  }

  "green returns value of third 8 bits only" in {
    forAll (byte, anyint) {
      (g, argb) => green(argb & 0xffff00ff | (g << 8)) shouldEqual g
    }
  }

  "blue returns value of first 8 bits only" in {
    forAll (byte, anyint) {
      (b, argb) => blue(argb & 0xffffff00 | b) shouldEqual b
    }
  }

  "rgba constructs proper ARGBs from elements" in {
    forAll (quadruple) {
      (q) => val pixel = ARGB(q._1, q._2, q._3, q._4)
        alpha(pixel) shouldEqual q._1
        red(pixel) shouldEqual q._2
        green(pixel) shouldEqual q._3
        blue(pixel) shouldEqual q._4
    }
  }

  "rgba constructs proper ARGBs from quadruples" in {
    forAll (quadruple) {
      (q) => val pixel = ARGB(q)
        alpha(pixel) shouldEqual q._1
        red(pixel) shouldEqual q._2
        green(pixel) shouldEqual q._3
        blue(pixel) shouldEqual q._4
    }
  }
}
