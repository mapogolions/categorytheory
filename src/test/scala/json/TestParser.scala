package io.github.mapogolions.json

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo


import io.github.mapogolions.json.ops.ParserOps._
import io.github.mapogolions.json.adt._
import io.github.mapogolions.json.functor.Functor
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._
import io.github.mapogolions.json.applicative.Applicative
import io.github.mapogolions.json.applicative.ApplicativeInstances._
import io.github.mapogolions.json.applicative.ApplicativeSyntax._

class TestParser {
  @Test
  def TestParseLift: Unit = {
    val pa = Applicative[Parser] pure 10
    val pb = Applicative[Parser] pure 'a'
    val f: Int => Char => String = a: Int => b: Char => a.toString + b + ""
    val fake = ""
    assertEquals(
      lift(f)(pa)(pb) apply fake,
      Success("10a", "")
    )
  }

  @Test
  def TestParseThreeDigitsAsInt: Unit = {
    assertEquals(
      parserThreeDigitsAsInt apply "1036number",
      Success(103, "6number")
    )
  }

  @Test
  def TestParseThreeDigitsAsStr: Unit = {
    assertEquals(
      parseThreeDigitsAsStr apply "103number",
      Success("103", "number")
    )
  }

  @Test
  def TestParseThreeDigits: Unit = {
    assertEquals(
      parseThreeDigits apply "103number",
      Success((('1', '0'), '3'), "number")
    )
    assertEquals(
      parseThreeDigits apply "",
      Failure("No more")
    )
  }

  @Test
  def TestParseUpperCase: Unit = {
    assertEquals(
      parseUpperCase apply "HEllo",
      Success('H', "Ello")
    )
  }

  @Test
  def TestParseLowerCase: Unit = {
    assertEquals(
      parseLowerCase apply "hellO",
      Success('h', "ellO")
    )
  }

  @Test
  def TestParseDigit: Unit = {
    assertEquals(
      parseDigit apply "10bar",
      Success('1', "0bar")
    )
    assertEquals(
      parseDigit apply "0hello",
      Success('0', "hello")
    )
  }

  @Test
  def TestMoreDifficultCases: Unit = {
    // Left associative
    val dAndBorF = 'd'.parse >> ('b'.parse <|> 'f'.parse)
    assertEquals(
      dAndBorF apply "dfoo",
      Success(('d', 'f'), "oo"),
    )
    assertEquals(
      dAndBorF apply "dbar",
      Success(('d', 'b'), "ar")
    )
    assertEquals(
      dAndBorF apply "",
      Failure("No more")
    )
  }

  @Test
  def TestorElse: Unit = {
    assertEquals(
      'a'.parse <|> 'b'.parse apply "aloha",
      Success('a', "loha")
    )
    assertEquals(
      'a'.parse <|> 'b'.parse apply "bloha",
      Success('b', "loha")
    )
    assertEquals(
      'a'.parse <|> 'b'.parse apply "abloha",
      Success('a', "bloha")
    )
    assertEquals(
      'a'.parse <|> 'b'.parse apply "",
      Failure("No more")
    )
  }

  @Test
  def TestAndThen: Unit = {
    assertEquals(
      ('b' parse) >> ('c' parse) apply "bcla-la-la",
      Success(('b', 'c'), "la-la-la")
    )
    assertEquals(
      ('b' parse) >> ('d' parse) apply "",
      Failure("No more")
    )
  }
}