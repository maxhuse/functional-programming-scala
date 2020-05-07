package functional

import functional.Chapter2._
import org.junit.Assert.assertEquals
import org.junit._

object Chapter2Test {

  class isSorted_2_2 {
    @Test def `isSorted empty`: Unit =
      assertEquals(
        true,
        isSorted(Array(), (a: Int, b: Int) => a.compareTo(b) <= 0)
      )

    @Test def `isSorted with length = 1`: Unit =
      assertEquals(
        true,
        isSorted(Array(1), (a: Int, b: Int) => a.compareTo(b) <= 0)
      )

    @Test def `isSorted with sorted array`: Unit =
      assertEquals(
        isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a.compareTo(b) <= 0),
        true
      )

    @Test def `isSorted with not sorted array`: Unit =
      assertEquals(
        false,
        isSorted(Array(1, 2, 3, 4, 0), (a: Int, b: Int) => a.compareTo(b) <= 0)
      )

    @Test def `isSorted with array of equals`: Unit =
      assertEquals(
        true,
        isSorted(Array(1, 1, 1, 1, 1), (a: Int, b: Int) => a.compareTo(b) <= 0)
      )

  }

  class Compose_2_5 {

    val f = (x: Int) => x * 2
    val g = (x: Int) => x + 10

    @Test def `compose`: Unit =
      assertEquals(
        (f compose g)(3),
        Chapter2.compose(f, g)(3)
      )

  }

}

