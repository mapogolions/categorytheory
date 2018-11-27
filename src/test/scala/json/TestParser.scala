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
  def TestKeepRight: Unit = {
    assertEquals(
      digit  <| digit | "123",
      Success('1', "3")
    )

    assertEquals(
      'a'.once |> 'b'.once | "abba",
      Success('b', "ba")
    )
    assertEquals(
      'a'.once |> 'b'.atLeastOne | "abba",
      Success(List('b', 'b'), "a")
    )
  }

  @Test
  def TestKeepLeft: Unit = {
    assertEquals(
      'a'.once <| 'b'.once apply "abba",
      Success('a', "ba")
    )
    assertEquals(
      'a'.once <| 'b'.atLeastOne | "abba",
      Success('a', "a")
    )
  }

  @Test
  def TestOpt: Unit = {
    assertEquals(
      'a'.parse.opt | "alloha", 
      Success(Some('a'), "lloha")
    )
    assertEquals(
      "ll".opt | "lloha",
      Success(Some("ll"), "oha")
    )
    assertEquals(
      pint.opt | "233h3",
      Success(Some(233), "h3")
    )
  }
  @Test
  def TestPint: Unit = {
    assertEquals(pint | "123hello", Success(123, "hello"))
    assertEquals(pint | "1020", Success(1020, ""))
    assertEquals(pint | "-123...", Success(-123, "..."))
    assertEquals(pint | "3434...", Success(3434, "..."))
  }

  @Test
  def TestUppserCase: Unit = {
    assertEquals(upperCase.once | "Albus", Success('A', "lbus"))
    assertEquals(upperCase.many | "lower", Success(Nil, "lower"))
    assertEquals(
      upperCase.atLeastOne | "OOps", 
      Success(List('O', 'O'), "ps")
    )
  }

  @Test
  def TestDigit: Unit = {
    assertEquals(
      digit.atLeastOne | "134",
      Success(List('1', '3', '4'), "")
    )
    assertEquals(
      digit.many apply "text",
      Success(Nil, "text")
    )
  }

  @Test
  def TestAtLeastOne: Unit = {
    assertEquals(
      "w".atLeastOne apply "www.google.com",
      Success(List("w", "w", "w"), ".google.com")
    )
    assertEquals(
      'a'.atLeastOne | "abus",
      Success(List('a'), "bus")
    )
  }

  @Test
  def TestMany: Unit = {
    assertEquals(
      digit.many apply "123hello",
      Success(List('1', '2', '3'), "hello")
    )
    
    assertEquals(
      "www".many("file://"),
      Success(Nil, "file://")
    )
    assertEquals(
      'b'.many apply "",
      Success(Nil, "")
    )
    assertEquals(
      "vaadin".many apply "vaadinvaadin vv",
      Success(List("vaadin", "vaadin"), " vv")
    )
    assertEquals(
      'a'.many | "aaabus",
      Success(List('a', 'a', 'a'), "bus")
    )
  }

  @Test
  def TestOnce: Unit = {
    assertEquals(
      'a'.once apply "alive",
      Success('a', "live")
    )
    assertEquals(
      "fun".once("functor"),
      Success("fun", "ctor")
    )
    assertEquals(
      'b'.once("boom"),
      Success('b', "oom")
    )
  }

  @Test
  def TestParseString: Unit = {
    val parseABC = "ABC".parse
    assertEquals(
      parseABC apply "ABCDE",
      Success("ABC", "DE")
    )
  }

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
  def TestOrElse: Unit = {
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