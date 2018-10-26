import io.github.mapogolions.categorytheory._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo


class BifunctorTest {
  @Test
  def bimapPreserveComposition(): Unit = {
    assertEquals(
      (Bifunctor.bisecondPair(Bifunctor.g) compose Bifunctor.bifirstPair(Bifunctor.f))(Pair(10, 'x')),
      Bifunctor.bimapPair(Bifunctor.f)(Bifunctor.g)(Pair(10, 'x'))
    )
    assertEquals(
      (Bifunctor.bifirstPair(Bifunctor.f) compose Bifunctor.bisecondPair(Bifunctor.g))(Pair(10, 'x')),
      Bifunctor.bimapPair(Bifunctor.f)(Bifunctor.g)(Pair(10, 'x'))
    )
  }

  @Test
  def bisecondAtTheSameThatBifunctorIdentityA(): Unit = {
    assertEquals(
      Bifunctor.bisecondPair(Bifunctor.g)(Pair(10, 'x')),
      Bifunctor.bimapPair(Bifunctor.id)(Bifunctor.g)(Pair(10, 'x'))
    )
    assertEquals(
      Bifunctor.bisecondPair(Bifunctor.g)(Pair(0, 'y')),
      Bifunctor.bimapPair(Bifunctor.id)(Bifunctor.g)(Pair(0, 'y'))
    )
  }

  @Test
  def bifirstAtTheSameThatBifunctorIdentityB(): Unit = {
    assertEquals(
      Bifunctor.bifirstPair(Bifunctor.f)(Pair(10, 'x')),
      Bifunctor.bimapPair(Bifunctor.f)(Bifunctor.id)(Pair(10, 'x'))
    )
    assertEquals(
      Bifunctor.bifirstPair(Bifunctor.f)(Pair(0, 'x')),
      Bifunctor.bimapPair(Bifunctor.f)(Bifunctor.id)(Pair(0, 'x'))
    )
  }

  @Test
  def bisecondSimpleTest(): Unit = {
    assertEquals(
      Bifunctor.bisecondPair(Bifunctor.g)(Pair(10, 'x')),
      Pair(10, "x")
    )
    assertEquals(
      Bifunctor.bisecondPair(Bifunctor.g)(Pair(0, 'y')),
      Pair(0, "y")
    )
  }

  @Test
  def bifirstSimpleTest(): Unit = {
    assertEquals(
      Bifunctor.bifirstPair(Bifunctor.f)(Pair(10, 'x')),
      Pair(true, 'x')
    )
    assertEquals(
      Bifunctor.bifirstPair(Bifunctor.f)(Pair(0, 'x')),
      Pair(false, 'x')
    )
    assertEquals(
      Bifunctor.bifirstPair(Bifunctor.f)(Pair(-10, 'x')),
      Pair(true, 'x')
    )
  }

  @Test
  def bimapSimpleTest(): Unit = {
    // `Either`
    assertEquals(
      Bifunctor.bimapEither(Bifunctor.f)(Bifunctor.g)(Left(10)),
      Left(true)
    )
    assertEquals(
      Bifunctor.bimapEither(Bifunctor.g)(Bifunctor.f)(Right(0)),
      Right(false)
    )

    // `Pair`
    assertEquals(
      Bifunctor.bimapPair(Bifunctor.f)(Bifunctor.g)(Pair(10, 'x')),
      Pair(true, "x")
    )
    assertEquals(
      Bifunctor.bimapPair(Bifunctor.f)(Bifunctor.g)(Pair(0, 'y')),
      Pair(false, "y")
    )
  }

  @Test
  def secondCategoryTest(): Unit = {
    assertEquals(Bifunctor.g('x'), "x")
    assertEquals(Bifunctor.g('a'), "a")
  }

  @Test
  def firstCategoryTest(): Unit = {
    assertTrue(Bifunctor.f(10))
    assertFalse(Bifunctor.f(0))
    assertTrue(Bifunctor.f(-10))
  }
}
