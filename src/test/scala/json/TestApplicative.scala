package io.github.mapogolions.json

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo

import io.github.mapogolions.json.adt._
import io.github.mapogolions.json.applicative.Applicative
import io.github.mapogolions.json.functor.Functor

class TestApplicative {
  import io.github.mapogolions.json.applicative.ApplicativeSyntax._
  import io.github.mapogolions.json.applicative.ApplicativeInstances._
  import io.github.mapogolions.json.functor.FunctorSyntax._
  import io.github.mapogolions.json.functor.FunctorInstances._
  import io.github.mapogolions.json.ops.ParserOps._


  @Test
  def TestApMultyArity: Unit = {
    val sum: Char => Int => Int = a: Char => b: Int => a.toInt + b
    assertEquals(
      Applicative[Parser].pure(sum) ap ('a' parse) ap ('b'.parse.map(_ toInt)) apply "abba",
      Success(97 + 98, "ba")
    )
  }

  @Test
  def TestApOnceArity: Unit = {
    val ff = Applicative[Parser] pure { x: Char => x toString }
    assertEquals(
      ff ap ('a' parse) apply "allo",
      Success("a", "llo")
    )
  }

  @Test
  def TestPure: Unit = {
    import io.github.mapogolions.json.ops.ParserOps._
    
    
    val inc: Int => Int = _ + 1
    val even: Int => Boolean = _ % 2 == 0
    val stringify: Boolean => String = _ toString
    val pipe = stringify compose even compose inc

    assertEquals(
      Applicative[Parser] pure pipe  apply "...",
      Success(pipe, "...")
    )

    assertEquals(
      Applicative[Parser] pure inc apply "...",
      Success(inc, "...")
    )


    assertEquals(
      ('t' parse) apply "toy story",
      Applicative[Parser] pure 't' apply "oy story"
    )

    assertEquals(
      Applicative[Parser] pure 'a' apply "hello",
      Success('a', "hello")
    )
    assertEquals(
      Applicative[Parser] pure 10 apply "...",
      Success(10, "...")
    )
    
    assertEquals(
      Applicative[Parser] pure true apply "...",
      Success(true, "...")
    )
  }
}