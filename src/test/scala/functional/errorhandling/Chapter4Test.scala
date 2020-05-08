package functional.errorhandling

import org.junit.Assert.assertEquals
import org.junit._

object Chapter4Test {

  class OptionTest {
    import functional.errorhandling.Option
    import functional.errorhandling.Option._

    val some: Option[Int] = Some(2)
    val none: Option[Int] = None

    @Test def `Option map`: Unit = {
      assertEquals(
        Some(3),
        some.map(_ + 1)
      )

      assertEquals(
        None,
        none.map(_ + 1)
      )
    }

    @Test def `Option getOrElse`: Unit = {
      assertEquals(
        2,
        some.getOrElse(0)
      )

      assertEquals(
        0,
        none.getOrElse(0)
      )
    }

    @Test def `Option flatMap`: Unit = {
      assertEquals(
        Some(3),
        some.flatMap(v => Some(v + 1))
      )

      assertEquals(
        Some(3),
        some.flatMap2(v => Some(v + 1))
      )

      assertEquals(
        None,
        some.flatMap2(_ => None)
      )

      assertEquals(
        None,
        none.flatMap(v => Some(v + 1))
      )

      assertEquals(
        None,
        none.flatMap2(v => Some(v + 1))
      )

      assertEquals(
        None,
        none.flatMap(_ => None)
      )
    }

    @Test def `Option orElse`: Unit = {
      assertEquals(
        Some(2),
        some.orElse(Some(0))
      )

      assertEquals(
        Some(2),
        some.orElse2(Some(0))
      )

      assertEquals(
        Some(0),
        none.orElse(Some(0))
      )

      assertEquals(
        Some(0),
        none.orElse2(Some(0))
      )
    }

    @Test def `Option filter`: Unit = {
      assertEquals(
        Some(2),
        some.filter(_ > 0)
      )

      assertEquals(
        Some(2),
        some.filter2(_ > 0)
      )

      assertEquals(
        None,
        none.filter(_ > 0)
      )

      assertEquals(
        None,
        none.filter2(_ > 0)
      )

      assertEquals(
        None,
        some.filter(_ > 10)
      )
    }

    @Test def `Option map2`: Unit = {
      assertEquals(
        Some(5),
        map2(some, Some(3))(_ + _)
      )

      assertEquals(
        Some(5),
        map2ViaFlatMap(some, Some(3))(_ + _)
      )

      assertEquals(
        None,
        map2(none, Some(3))(_ + _)
      )

      assertEquals(
        None,
        map2ViaFlatMap(none, Some(3))(_ + _)
      )
    }

    @Test def `Option sequence`: Unit = {
      assertEquals(
        Some(List(3, 2)),
        sequence(List(Some(3), Some(2)))
      )

      assertEquals(
        Some(List(3, 2)),
        sequenceViaTraverse(List(Some(3), Some(2)))
      )

      assertEquals(
        None,
        sequence(List(Some(3), None))
      )

      assertEquals(
        None,
        sequenceViaTraverse(List(Some(3), None))
      )
    }

    @Test def `Option traverse`: Unit = {
      assertEquals(
        Some(List(4, 3)),
        traverse(List(3, 2))(v => Some(v + 1))
      )

      assertEquals(
        Some(List(4, 3)),
        traverse(List(Some(3), Some(2)))(v => v.map(_ + 1))
      )

      assertEquals(
        None,
        traverse(List(Some(3), None))(v => v.map(_ + 1))
      )

      assertEquals(
        None,
        traverse(List(3, 2))(v => None)
      )

      assertEquals(
        Some(Nil),
        traverse(List[Int]())(v => Some(v))
      )
    }
  }

  class EitherTest {
    import functional.errorhandling.Either
    import functional.errorhandling.Either._

    val right: Either[String, Int] = Right(3)
    val left: Either[String, Int] = Left("error")

    @Test def `Either map`: Unit = {
      assertEquals(
        Right(4),
        right.map(_ + 1)
      )

      assertEquals(
        left,
        left.map(_ + 1)
      )
    }

    @Test def `Either flatMap`: Unit = {
      assertEquals(
        Right(4),
        right.flatMap(v => Right(v + 1))
      )

      assertEquals(
        Left("bark"),
        right.flatMap(v => Left("bark"))
      )

      assertEquals(
        left,
        left.flatMap(v => Right(v + 1))
      )

      assertEquals(
        Left("error"),
        left.flatMap(v => Left("bark"))
      )
    }

    @Test def `Either orElse`: Unit = {
      assertEquals(
        Right(3),
        right.orElse(Right(0))
      )

      assertEquals(
        Right(0),
        left.orElse(Right(0))
      )
    }

    @Test def `Either map2`: Unit = {
      assertEquals(
        Right(5),
        right.map2(Right(2))(_ + _)
      )

      assertEquals(
        left,
        right.map2(left)(_ + _)
      )

      assertEquals(
        left,
        left.map2(Right(0))(_ + _)
      )
    }

    @Test def `Either traverse`: Unit = {
      assertEquals(
        Right(List(3, 4)),
        traverse(List(2, 3))(v => Right(v + 1))
      )

      assertEquals(
        Left("bark"),
        traverse(List(2, 3))(v => Left("bark"))
      )
    }

    @Test def `Either sequence`: Unit = {
      assertEquals(
        Right(List(2, 3)),
        sequence(List(Right(2), Right(3)))
      )

      assertEquals(
        left,
        sequence(List(Right(2), left))
      )
    }

  }

}
