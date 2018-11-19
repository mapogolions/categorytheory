package io.github.mapogolions.cats

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo

import io.github.mapogolions.cats.applicative.Applicative
import io.github.mapogolions.cats.adt._

class TestApplicative {
  import io.github.mapogolions.cats.applicative.ApplicativeInstances._
  import io.github.mapogolions.cats.applicative.ApplicativeSyntax._

  @Test
  def mapTest: Unit = {
    assertEquals(
      List(10, -2).map(_ > 0),
      List(true, false)
    )

    assertEquals(
      Applicative[Maybe].map(Just(10))({ a: Int => a > 0 }),
      Just(true)
    )
    assertEquals(
      Applicative[List].map(List(1, 2, 3, 4, 5, 6))({ a: Int => a % 2 == 0 }),
      List(false, true, false, true, false, true)
    )
  }

  @Test
  def pureTest: Unit = {
    assertEquals(Applicative[Maybe].pure(10), Just(10))
    assertEquals(Applicative[List].pure(10), List(10))
    assertEquals(Applicative[List].pure(10), Cons(10, Nil))
  }
}