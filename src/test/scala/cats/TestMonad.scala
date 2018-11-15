package io.github.mapogolions.cats

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo

import io.github.mapogolions.cats.monad.Monad
import io.github.mapogolions.cats.adt.{
  List, Cons, Nil, Maybe, Just, Blank, Pair, Either, Left, Right
}


class TestMonad {
  import io.github.mapogolions.cats.monad.MonadInstances._
  import io.github.mapogolions.cats.monad.MonadSyntax._


  @Test
  def fishOperator(): Unit = {
    /* Fish operator  (f >=> g) */
    val f: String => List[(Char, String)] = msg => List((msg(0), msg.substring(1)))
    val g: ((Char, String)) => List[(Int, String)] = (ch, msg) => List((ch.toInt, msg))
    val h: ((Int, String)) => List[String] = (n, msg) => List((n + 1).toChar + msg)

    assertEquals(
      ({ a: Int => List(a > 0)} >=> { b: Boolean => List(b.toString) })(1),
      List("true")
    )
    assertEquals(
      (f >=> g >=> h)("Jotlin"),
      Cons("Kotlin", Nil)
    )
    assertEquals(
      ({ n: Int => Just(n > 0) } >=> { flag: Boolean => Just(flag.toString) })(0),
      Just("false")
    )
  }

  @Test
  def bindOperator(): Unit = {
    assertEquals(
      List(1, 2, -2, 0, 10) ->> { a: Int => List(a > 0) },
      List(true, true, false, false, true)
    )
    assertEquals(
      List(93, 45, 44) ->> { a: Int => List(a.toChar) },
      List(']', '-', ',')
    )
    assertEquals(
      Cons(1, Cons(0, Nil)) ->> { a: Int => Cons(a > 0, Nil) },
      List(true, false)
    )
    assertEquals( 
      Nil ->> { a: Char => Cons(a.toChar, Nil) },
      Nil
    )
    assertEquals(
      Just(10) ->> { n => Just(n > 0) },
      Just(true)
    )
    assertEquals(
      Just('w') ->> { ch => Just(ch + "orld") },
      Just("world")
    )
    assertEquals( 
      Blank ->> { a: Int => Just( a > 0) },
      Blank
    )
  }

  @Test
  def firstDraftTest(): Unit = {
    assertTrue(true)
  }
}