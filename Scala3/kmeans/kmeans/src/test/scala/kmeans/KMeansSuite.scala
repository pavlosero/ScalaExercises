package kmeans

import java.util.concurrent.*
import scala.collection.{mutable, Map, Seq}
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.parallel.CollectionConverters.*
import scala.math.*

class KMeansSuite extends munit.FunSuite:
  object KM extends KMeans
  import KM.*

  def checkParClassify(points: ParSeq[Point], means: ParSeq[Point], expected: ParMap[Point, ParSeq[Point]]): Unit =
    assertEquals(classify(points, means), expected, s"classify($points, $means) should equal to $expected")

  trait TestPoints {
    val points = Seq(Point(0, 0, 0), Point(1, 1, 1), Point(2,2, 2)).par
    val means = Seq(Point(0, 0, 0), Point(1, 1, 1), Point(2,2, 2)).par
  }
  test("'classify' should work for empty 'points' and empty 'means'") {
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq().par
    val expected = ParMap[Point, ParSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test("'classify' 3 points to 3 means") {
    new TestPoints {
      val expected = Map(means(0) -> ParSeq(points(0)),
                          means(1) -> ParSeq(points(1)),
                          means(2) -> ParSeq(points(2))).par
      checkParClassify(points, means, expected)
    }
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds


