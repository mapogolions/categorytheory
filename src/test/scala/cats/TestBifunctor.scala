package io.github.mapogolions.cats

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo

import io.github.mapogolions.cats.bifunctor.Bifunctor
import io.github.mapogolions.cats.adt.{
  List, Cons, Nil, Maybe, Just, Blank, Pair, Either, Left, Right
}


class TestBifunctor {
  import io.github.mapogolions.cats.bifunctor.BifunctorInstances._
  import io.github.mapogolions.cats.bifunctor.BifunctorSyntax._

  @Test
  def leftBifunctorTest: Unit = {
    val FL = Bifunctor[Pair].leftFunctor[Boolean]
    assertEquals(FL.map(Pair("hello", false))(_.length), Pair(5, false))

    val FL2 = Bifunctor[Either].leftFunctor[Int]
    assertEquals(FL2.map(Left(10))(_ > 0), Left(true))
  }

  @Test
  def rightFunctorTest: Unit = {
    val FR = Bifunctor[Pair].rightFunctor[Char]
    assertEquals(FR.map(Pair('a', 10))(_ > 0), Pair('a', true))
  }

  @Test
  def firstTest(): Unit = {
    assertThat(
      Pair(0, false),
      allOf(
        equalTo( (Pair(-1, false) first { ~_ })() ),
        equalTo( Pair (-1, false).lift({ ~_ }, identity)() )
      )
    )
    assertEquals(
      (Pair(-1, false) first { ~_ })(),
      Pair(0, false)
    )
  }

  @Test
  def liftTest(): Unit = {
    assertThat(
      Pair(false, true),
      allOf(
        equalTo(Pair(-1, 'w').lift({ _ > 0 }, { _ < 'z' })()),
        equalTo(Pair(-1, 'w').bimap({ _ > 0 }, { _ < 'z' }))
      )
    )
    assertEquals(
      (Pair(-1, 'w') lift ({ _ > 0 }, { _ < 'z' }))(),
      Pair(false, true)
    )
  }

  @Test
  def rightMapTest(): Unit = {
    assertThat(
      Pair("", false),
      allOf(
        equalTo(Pair("", -1) bimap (identity, { _ > 0 })),
        equalTo( Bifunctor[Pair].rightMap(Pair("", -1))(_ > 0) )
      )
    )
  }

  @Test
  def leftMapTest(): Unit = {
    assertThat(
      Pair(true, 0),
      allOf(
        equalTo(Pair(false, 0) bimap ({ !_ }, identity)),
        equalTo(Bifunctor[Pair].bimap(Pair(false, 0))({ !_ }, identity) ),
        equalTo(Bifunctor[Pair].leftMap(Pair(false, 0))(!_)),
      )
    )
    assertEquals(
      Pair(10, "") leftMap { _ % 2 == 0 },
      Pair(10, "") bimap ({ _ % 2 == 0 }, identity)
    )

    assertEquals(
      Bifunctor[Pair].leftMap(Pair(45, ""))(_ % 2 == 0),
      Bifunctor[Pair].bimap(Pair(45, ""))({ _ % 2 == 0}, identity)
    )
  }

  @Test
  def bimapTest(): Unit = {
    assertEquals(
      Right(10) bimap (identity, { _ < 10 }),
      Right(false)
    )
    assertEquals(
      Left(10) bimap ({ _ > 10 }, identity),
      Left(false)
    )
    assertEquals(
      Bifunctor[Pair].bimap(Pair(100, false))({ _.toChar}, { !_ }),
      Pair('d', true)
    )
    assertEquals(
      Pair('c', true) bimap ({ _ + 1 }, { _.toString }),
      Pair('d', "true")
    )
    assertEquals(
      Pair(10, 'M') bimap ({ _ > 0 }, { _ + "ost" }),
      Pair(true, "Most")
    )
  }

  @Test
  def umapTest(): Unit = {
    assertEquals(
      Pair(-10, 30) umap { _ > 0 },
      Pair(false, true)
    )
    assertEquals(
      Pair('a', 'k') umap { _ < 'b' },
      Pair(true, false)
    )
  }

  @Test
  def firstDraftTest(): Unit = {
    assertTrue(true)
  }
}
