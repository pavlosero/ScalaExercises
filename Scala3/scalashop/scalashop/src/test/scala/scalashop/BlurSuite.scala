package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite {
  // Put tests here
  def setImgVals(src: Img): Img = {
    for (i <- 0 until src.width; j <- 0 until src.height) {
      val s = i*10*src.width+j
      src(i,j) = rgba(red(s), green(s), blue(s), alpha(s))
    }
    src
  }


  trait TestImage {
      val img = setImgVals(Img(3,3))
  }

  test("blurBox with 0 radius") {
    new TestImage {
      assert(boxBlurKernel(img, 1, 1, 0) == img(1,1))
    }
  }

  test("blurBox with radius1") {
    new TestImage {
      val a = boxBlurKernel(img, 0 , 1, 1)
    }
  }
}
