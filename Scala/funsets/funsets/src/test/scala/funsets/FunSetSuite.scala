package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val us = union(union(s1,s2),s3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton 1")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intesection contains only elements belonging to both sets`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      val i = intersect(s, s1)
      assert(contains(i, 1), "1 is an element of intersection {1} and {1, 2}")
      assert(!contains(i, 2), "2 is not an element of intersection {1} and {1, 2}")
    }
  }

  @Test def `diff contains only elements of 1st set but not of the 2nd`: Unit = {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s1, s3)
      val d = diff(u1, u2)

      assert(contains(d, 2), "2 is an element of diff {1,2} and {1,3}")
      assert(!contains(d, 1), "1 is not an element of diff {1,2} and {1,3}")
      assert(!contains(d,3), "3  is not an element of diff {1,2} and {1,3}")
    }
  }

  @Test def `filter contains only elements of set that satisfy a predicate`: Unit = {
    new TestSets {
      val u1 = union(s1, s3)
      val u2 = union(s1, s2)
      val f1 = filter(u1, x => x>=2) // filter elements that lower then 2
      val f2 = filter(u2, x => x%2 != 0) // only odd

      assert(contains(f1, 3), "3 is an element greater then 2 in set {1,3}")
      assert(!contains(f1, 1), "1 is not an element greater then 2 in set {1,3}")
      assert(contains(f2, 1), "1 is an odd element in set {1,2}")
      assert(!contains(f2, 2), "2 is not an odd element in set {1,2}")
    }
  }


  @Test def `forall is true iff every element in set satisfy predicate`: Unit = {
    new TestSets {
      assert(forall(us, x => x>0), "all elements in set {1,2,3} are greater then 0")
      assert(!forall(us, x => x%2 == 0), "not all elements in set {1, 2, 3} are even")
    }
  }

  @Test def `exists is true iff there is at least one element in set satisfying predicate`: Unit = {
    new TestSets {
      assert(exists(us, x => x%2 == 0), "there is an odd element in set {1, 2, 3}")
      assert(exists(us, x => x>1), "there is an element greater then 1 in set {1, 2, 3}")
      assert(!exists(us, x => x>3), "there is not an element greater then 3 in set {1, 2, 3}")
    }
  }

  @Test def `map contains only elements results of apply function f`: Unit = {
    new TestSets {
      val m = map(us, x => x*x) // {1, 2, 3} => {1, 4, 9}

      assert(contains(m, 4), "4 is an element of set {1^2, 2^2, 3^2}")
      assert(!contains(m, 2), "2 is not an element of set {1^2, 2^2, 3^2}")
    }
  }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
