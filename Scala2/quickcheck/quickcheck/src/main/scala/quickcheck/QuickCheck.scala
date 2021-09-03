package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("twoInsert") = forAll { (a: A, b: A) =>
    val m = insert(b, insert(a, empty))
    findMin(m) == ord.min(a,b)
  }

  property("InsertAndDelete") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("isSortedByRepeat") = forAll { (h: H) =>
    def sortAsc(heap: H): List[A] = if (isEmpty(heap)) Nil else findMin(heap) :: sortAsc(deleteMin(heap))
    val sortedList = sortAsc(h)
    sortedList == sortedList.sorted
  }

  property("minMelted") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    findMin(m) == findMin(h1) || findMin(m) == findMin(h2)
  }

  property("correct melding") = forAll { (h1: H, h2: H) =>
    def isPresent(x: A, melted: H): Boolean = if (isEmpty(melted)) false else x == findMin(melted) || isPresent(x, deleteMin(melted))
    def isHeapPresent(h: H, melted: H): Boolean = {
      if (isEmpty(h)) true
      else if (isEmpty(melted)) false
      else isPresent(findMin(h), melted) && isHeapPresent(deleteMin(h), melted)
    }

    val m = meld(h1, h2)
    isHeapPresent(h1, m) && isHeapPresent(h2, m)
  }

