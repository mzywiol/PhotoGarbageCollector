package xz.similar

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.{Gen, Arbitrary}

class ARGBTest extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  val byte = Gen.choose(0, 255)
  val quadruple = for {
    a <- byte
    r <- byte
    g <- byte
    b <- byte
  } yield (a, r, g, b)

  "alpha returns value of first 8 bits only" in {
    forAll (byte, Gen.) {
      (a, argb) => alpha((a << 24) | argb)
    }
  }

  "rgba constructs proper ARGBs from elements" in {
    forAll (quadruple) {
      (q) => val pixel = rgba(q._1, q._2, q._3, q._4)
        alpha(pixel) shouldEqual q._1
        red(pixel) shouldEqual q._2
        green(pixel) shouldEqual q._3
        blue(pixel) shouldEqual q._4
    }
  }

  "rgba constructs proper ARGBs from quadruples" in {
    forAll (quadruple) {
      (q) => val pixel = rgba(q)
        alpha(pixel) shouldEqual q._1
        red(pixel) shouldEqual q._2
        green(pixel) shouldEqual q._3
        blue(pixel) shouldEqual q._4
    }
  }
}

class QuadTest extends WordSpec {
//  "sum adds all fields of two quads" in {
//  }
}
