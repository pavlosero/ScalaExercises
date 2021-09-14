package barneshut

import barneshut.FloatOps.FloatOps

import java.util.concurrent.*
import scala.collection.*
import scala.math.*
import scala.collection.parallel.*
import barneshut.conctrees.ConcBuffer
object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

class BarnesHutSuite extends munit.FunSuite:
  // test cases for quad tree

  def verifyEmpty(actual: Quad, centerX: Float, centerY: Float, size: Float): Unit = {
    actual match {
      case Empty(cx, cy, sz) =>
        assert(cx == centerX, s"$cx should be $centerX")
        assert(cy == centerY, s"$cy should be $centerY")
        assert(sz == size, s"$sz should be $size")
      case _ =>
        fail(s"$actual should have been Empty")
    }
  }

  def verifyLeaf(actual: Quad, centerX: Float, centerY: Float, size: Float, bodies: Seq[Body],
                 mass: Float, massX: Float, massY: Float, total: Int): Unit = {
    actual match {
      case Leaf(cx, cy, sz, bds) =>
        assert(cx == centerX, s"$cx should be $centerX")
        assert(cy == centerY, s"$cy should be $centerY")
        assert(sz == size, s"$sz should be $size")
        assert(bds.toSet == bodies.toSet, s"$bds toSet should be $bodies toSet")
        assert(actual.mass ~= mass, s"${actual.mass} should be $mass")
        assert(actual.massX ~= massX, s"${actual.massX} should be $massX")
        assert(actual.massY ~= massY, s"${actual.massY} should be $massY")
        assert(actual.total == total, s"${actual.total} should be $total")
      case _ =>
        fail(s"$actual should have been a Leaf")
    }
  }

  def verifyFork(actual: Quad, centerX: Float, centerY: Float, size: Float,
                 mass: Float, massX: Float, massY: Float, total: Int): Unit = {
    actual match {
      case Fork(_, _, _, _) =>
        assert(actual.centerX == centerX, s"${actual.centerX} should be $centerX")
        assert(actual.centerY == centerY, s"${actual.centerY} should be $centerY")
        assert(actual.mass ~= mass, s"${actual.mass} should be $mass")
        assert(actual.massX ~= massX, s"${actual.massX} should be $massX")
        assert(actual.massY ~= massY, s"${actual.massY} should be $massY")
        assert(actual.total == total, s"${actual.total} should be $total")
      case _ =>
        fail(s"$actual should have been a Fork")
    }
  }

  def verifyBody(actual: Body, mass: Float, x: Float, y: Float,
                 xspeed: Float, yspeed: Float): Unit = {
    assert(actual.mass == mass, s"${actual.mass} should be $mass")
    assert(actual.x ~= x, s"${actual.x} should be $x")
    assert(actual.y ~= y, s"${actual.y} should be $y")
    assert(actual.xspeed ~= xspeed, s"${actual.xspeed} should be $xspeed")
    assert(actual.yspeed ~= yspeed, s"${actual.yspeed} should be $yspeed")
  }

  def verifyBoundary(b: Boundaries, minX: Float, minY: Float, maxX: Float, maxY: Float): Unit = {
    assert(b.minX == minX, s"${b.minX} should be $minX")
    assert(b.minY == minY, s"${b.minY} should be $minY")
    assert(b.maxX == maxX, s"${b.maxX} should be $maxX")
    assert(b.maxY == maxY, s"${b.maxY} should be $maxY")
  }

  def verifySectorMatrix(actual: SectorMatrix,
                         expectedSectors: (Int, Int, Seq[Body])*): Unit = {
    expectedSectors.foreach{ case (x, y, sb) =>
      assert(actual(x, y).toSet == sb.toSet,
        s"Sector ($x, $y) has bodies ${actual(x, y)} rather than the expected bodies $sb")
    }
  }
  import FloatOps.*
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f ")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body (2pts)") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
  }

  // test cases for Body

  test("Body.updated should take bodies in a Leaf into account (2pts)") {
    val b1 = Body(123f, 18f, 26f, 0f, 0f)
    val b2 = Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96 (2pts)") {
    val body = Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    verifyLeaf(inserted, 51f, 46.3f, 5f, Seq(b), b.mass, b.x, b.y, 1)
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))
    verifyLeaf(quad, 17.5f, 27.5f, 5f, Seq(b), b.mass, b.x, b.y, 1)
  }

  test("Leaf with 2 bodies") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(234f, 22f, 14f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b1, b2))
    verifyLeaf(quad, 17.5f, 27.5f, 5f, Seq(b1, b2), 357f, 20.6218f, 18.1344f, 2)
  }

  test("Leaf.insert(b) should create a new Leaf when size <= maximumSize") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(234f, 22f, 14f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, minimumSize, Seq(b1))
    val inserted = quad.insert(b2)
    verifyLeaf(inserted, 17.5f, 27.5f, minimumSize, Seq(b1, b2), 357f, 20.6218f, 18.1344f, 2)
  }

  test("Leaf.insert(b) should create a Fork when size > maximumSize") {
    val b1 = new Body(123f, 14f, 30f, 0f, 0f)
    val b2 = new Body(234f, 22f, 27f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 10f, Seq(b1))
    val inserted = quad.insert(b2)

    inserted match {
      case Fork(nw, ne, sw, se) =>
        verifyEmpty(nw, 15f, 25f, 5f)
        verifyLeaf(ne, 20f, 25f, 5f, Seq(b2), b2.mass, b2.x, b2.y, 1)
        verifyLeaf(sw, 15f, 30f, 5f, Seq(b1), b1.mass, b1.x, b1.y, 1)
        verifyEmpty(se, 20f, 30f, 5f)
      case _ =>
        fail(s"$inserted should have been a Fork")
    }
  }

  test("Fork with all quadrants empty") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    verifyFork(quad, 20f, 30f, 10f, 0f, 20f, 30f, 0)
  }

  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    verifyFork(quad, 20f, 30f, 10f, 123f, 18f, 26f, 1)
  }

  test("Fork with 4 leafs") {
    val b1 = new Body(1f, 15f, 25f, 0f, 0f)
    val b2 = new Body(2f, 20f, 25f, 0f, 0f)
    val b3 = new Body(3f, 15f, 30f, 0f, 0f)
    val b4 = new Body(4f, 20f, 30f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1))
    val ne = Leaf(22.5f, 27.5f, 5f, Seq(b2))
    val sw = Leaf(17.5f, 32.5f, 5f, Seq(b3))
    val se = Leaf(22.5f, 32.5f, 5f, Seq(b4))
    val quad = Fork(nw, ne, sw, se)
    verifyFork(quad, 20f, 30f, 10f, 10f, 18f, 28.5f, 4)
  }

  test("Fork.insert(b) should create new Fork (with a new body in se)") {
    val b1 = new Body(3f, 18f, 26f, 0f, 0f)
    val b2 = new Body(7f, 20.5f, 30.5f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    val inserted = quad.insert(b2)
    verifyFork(inserted, 20f, 30f, 10f, 10f, 19.75f, 29.15f, 2)
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees and 0 speed") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    verifyBody(body, 123f, 18f, 26f, 0f, 0f)
  }

  test("Body.updated should update its x and y positions") {
    val b1 = new Body(123f, 18f, 26f, 3f, 5f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    verifyBody(body, 123f, 18f + 3f*delta, 26f + 5f*delta, 3f, 5f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    verifyBody(body, 123f, 18f, 26f, 12.587037f, 0.015557117f)
  }

  test("Body.updated should approximate a Fork that is far away with a single point of mass") {
    val b1 = new Body(123f, 5f, 5f, 0f, 0f)
    val b2 = new Body(222.2f, 47.5f, 42.5f, 0f, 0f)
    val b3 = new Body(333.3f, 55f, 55f, 0f, 0f)
    val nw = Leaf(45f, 45f, 10f, Seq(b2))
    val ne = Empty(55f, 45f, 10f)
    val sw = Empty(45f, 55f, 10f)
    val se = Leaf(55f, 55f, 10f, Seq(b3))
    val quad = Fork(nw, ne, sw, se)

    val body = b1.updated(quad)

    verifyBody(body, 123f, 5f, 5f, 0.094767f, 0.090734f)
  }

  test("Body.updated should recurse on each child quadtree of a Fork that is close by") {
    val b1 = new Body(123f, 50f, 50f, 0f, 0f)
    val b2 = new Body(222.2f, 47.5f, 42.5f, 0f, 0f)
    val b3 = new Body(333.3f, 55f, 55f, 0f, 0f)
    val nw = Leaf(45f, 45f, 10f, Seq(b2))
    val ne = Empty(55f, 45f, 10f)
    val sw = Empty(45f, 55f, 10f)
    val se = Leaf(55f, 55f, 10f, Seq(b3))
    val quad = Fork(nw, ne, sw, se)

    val body = b1.updated(quad)

    verifyBody(body, 123f, 50f, 50f, 3.589321f, 1.340815f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    verifySectorMatrix(sm, (2, 3, Seq(body)))
  }

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    verifySectorMatrix(sm, (1, 3, Seq(body)))
  }

  test("'SectorMatrix.+=' should add a body at (100,-12) to the bucket containing the closest point inside the boundary of a sector matrix of size 96") {
    val body = new Body(5, 100, -12, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    verifySectorMatrix(sm, (7, 0, Seq(body)))
  }

  test("'SectorMatrix.combine' should contain all bodies from both sector matrices") {
    val b1 = new Body(5, 25, 47, 0.1f, 0.1f)
    val b2 = new Body(5, 30, 37, 0.1f, 0.1f)
    val b3 = new Body(5, 13, 97, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm1 += b1
    sm2 += b2
    sm2 += b3
    val sm3 = sm1.combine(sm2)
    verifySectorMatrix(sm3, (2, 3, Seq(b1, b2)), (1, 7, Seq(b3)))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds


