package functional.laziness

import org.junit.Assert.assertEquals
import org.junit._

class Chapter5Test {
  import functional.laziness.Stream
  import functional.laziness.Stream._

  val sConstant: Stream[Int] = constant(42)
  val sFrom: Stream[Int] = from(1)

  @Test def `Stream take`: Unit = {
    assertEquals(
      List(1, 2, 3),
      sFrom.take(3).toList
    )

    assertEquals(
      List(1, 2, 3),
      sFrom.takeViaUnfold(3).toList
    )
  }

  @Test def `Stream drop`: Unit = {
    assertEquals(
      List(4, 5, 6, 7),
      sFrom.drop(3).take(4).toList
    )
  }

  @Test def `Stream takeWhile`: Unit = {
    assertEquals(
      List(1, 2, 3),
      sFrom.takeWhile(_ < 4).toList
    )

    assertEquals(
      List(1, 2, 3, 4),
      sFrom.takeWhileViaFold(_ < 5).toList
    )

    assertEquals(
      List(1, 2, 3, 4, 5),
      sFrom.takeWhileViaUnfold(_ < 6).toList
    )
  }

  @Test def `Stream forAll`: Unit = {
    assertEquals(
      true,
      sFrom.take(4).forAll(_ <= 4)
    )
  }

  @Test def `Stream headOption`: Unit = {
    assertEquals(
      Some(5),
      sFrom.drop(4).headOption
    )
  }

  @Test def `Stream map`: Unit = {
    assertEquals(
      List(2, 4, 6, 8),
      sFrom.map(_ * 2).take(4).toList
    )

    assertEquals(
      List(2, 4, 6, 8),
      sFrom.mapViaUnfold(_ * 2).take(4).toList
    )
  }

  @Test def `Stream filter`: Unit = {
    assertEquals(
      List(2, 4, 6, 8),
      sFrom.filter(_ % 2 == 0).take(4).toList
    )
  }

  @Test def `Stream append`: Unit = {
    assertEquals(
      List(1, 2, 3, 4, 3, 2, 1),
      sFrom.take(4).append(Stream(3, 2, 1)).toList
    )
  }

  @Test def `Stream flatMap`: Unit = {
    assertEquals(
      List(2, 4, 6, 8),
      sFrom.flatMap(a => Stream(a * 2)).take(4).toList
    )
  }

  @Test def `Stream zipWith`: Unit = {
    assertEquals(
      List(43, 44, 45, 46),
      sFrom.zipWith(sConstant)(_ + _).take(4).toList
    )
  }

  @Test def `Stream zipAll`: Unit = {
    assertEquals(
      List((Some(1), Some(3)), (Some(2), Some(2)), (Some(3), Some(1)), (Some(4), None)),
      sFrom.zipAll(Stream(3, 2, 1)).take(4).toList
    )
  }

  @Test def `Stream startsWith`: Unit = {
    assertEquals(
      true,
      sFrom.startsWith(Stream(1, 2, 3))
    )

    assertEquals(
      false,
      sFrom.startsWith(Stream(3, 2, 1))
    )
  }

  @Test def `Stream hasSubsequence`: Unit = {
    assertEquals(
      true,
      sFrom.hasSubsequence(Stream(10, 11, 12))
    )

    assertEquals(
      false,
      sFrom.take(100).hasSubsequence(Stream(10, 12, 13))
    )
  }

  @Test def `Stream scanRight`: Unit = {
    assertEquals(
      List(10, 9, 7, 4, 0),
      sFrom.take(4).scanRight(0)(_ + _).toList
    )
  }
}
