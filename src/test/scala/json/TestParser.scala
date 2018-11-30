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
  def TestSep: Unit = {
    ('?'.sep(whitespace) | "? ? ?...").test(
      (elem: List[Char], src: Source) => assertEquals(elem, List('?', '?', '?')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestWhitespaces: Unit = {
    (whitespaces | "\t text").test(
      (elem: List[Char], src: Source) => assertEquals(elem, List('\t', ' ')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (whitespaces | "\n\t text").test(
      (elem: List[Char], src: Source) => assertEquals(elem, List('\n', '\t', ' ')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestWhitespace: Unit = {
    (whitespace | " text").test(
      (elem: Char, src: Source) => assertEquals(elem, ' '),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (whitespace | "\t text").test(
      (elem: Char, src: Source) => assertEquals(elem, '\t'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (whitespace | "\n\t text").test(
      (elem: Char, src: Source) => assertEquals(elem, '\n'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ((whitespace |> digit).many | " 1 2 3 ").test(
      (elem: List[Char], src: Source) => 
        assertEquals(elem, List('1', '2', '3')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (('-'.once <| digit).many | "-1-2-3-").test(
      (elem: List[Char], src: Source) => 
        assertEquals(elem, List('-', '-', '-')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestBetween: Unit = {
    ('a'.between('b'.once)('c'.once) | "bac-...").test(
      (elem: Char, src: Source) => assertEquals(elem, 'a'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestKeepRight: Unit = {
    (digit  |> digit | "123").test(
      (elem: Char, src: Source) => assertEquals(elem, '2'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ('a'.once  |> 'b'.once | "abba").test(
      (elem: Char, src: Source) => assertEquals(elem, 'b'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestKeepLeft: Unit = {
    ('a'.once <| 'b'.once | "abba").test(
      (elem: Char, src: Source) => assertEquals(elem, 'a'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (digit  <| digit | "123").test(
      (elem: Char, src: Source) => assertEquals(elem, '1'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ('a'.once <| 'b'.atLeastOne | "abba").test(
      (elem: Char, src: Source) => assertEquals(elem, 'a'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestOpt: Unit = {
    ('a'.parse.opt | "alloha").test(
      (elem: Option[Char], src: Source) => assertEquals(elem, Some('a')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ("ll".parse.opt | "lloha").test(
      (elem: Option[String], src: Source) => assertEquals(elem, Some("ll")),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (pint.opt | "233h3").test(
      (elem: Option[Int], src: Source) => assertEquals(elem, Some(233)),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }
  
  @Test
  def TestPint: Unit = {
    (pint | "123hello").test(
      (elem: Int, src: Source) => assertEquals(elem, 123),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (pint | "-1020").test(
      (elem: Int, src: Source) => assertEquals(elem, -1020),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestDigit: Unit = {
    (digit.atLeastOne | "134").test(
      (elem: List[Char], src: Source) => assertEquals(elem, List('1', '3', '4')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (digit.many | "text").test(
      (elem: List[Char], src: Source) => assertEquals(elem, Nil),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestAtLeastOne: Unit = {
    ("w".atLeastOne | "www.google.com").test(
      (elem: List[String], src: Source) => assertEquals(elem, List("w", "w", "w")),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ('a'.atLeastOne | "abus").test(
      (elem: List[Char], src: Source) => assertEquals(elem, 'a' :: Nil),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestMany: Unit = {
    (digit.many | "123hello").test(
      (elem: List[Char], src: Source) => assertEquals(elem, List('1', '2', '3')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ("www".many | "file://").test(
      (elem: List[String], src: Source) => assertEquals(elem, Nil),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ('b'.many | "").test(
      (elem: List[Char], src: Source) => assertEquals(elem, Nil),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestOnce: Unit = {
    ('a'.once | "alive").test(
      (elem: Char, src: Source) => assertEquals(elem, 'a'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    ("fun".once | "functor").test(
      (elem: String, src: Source) => assertEquals(elem, "fun"),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestParseString: Unit = {
    val parseABC = "ABC".parse
    (parseABC | "ABCDE").test(
      (elem: String, src: Source) => assertEquals(elem, "ABC"),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestUppserCase: Unit = {
    (upper | "Albus").test(
      (elem: Char, src: Source) => assertEquals(elem, 'A'),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )

    (upper.atLeastOne | "OOps").test(
      (elem: List[Char], src: Source) => assertEquals(elem, List('O', 'O')),
      (label: String, err: String, pos: Position) => assertFail("Should be success")
    )
  }

  @Test
  def TestMoreDifficultCases: Unit = {
    // Left associative
    val dAndBorF = 'd'.parse >> ('b'.parse <|> 'f'.parse)
    assertEquals(
      dAndBorF | "dfoo" toString,
      "(d,f)"
    )
    assertEquals(
      dAndBorF | "dbar" toString,
      "(d,b)"
    )

    (dAndBorF | "dfoo")
      .test(
        (elem: (Char, Char), src: Source) => assertEquals(elem, ('d', 'f')),
        (label: String, err: String, pos: Position) => assertFail("Should be success")
      )
  }

  @Test
  def TestOrElse: Unit = {
    assertEquals(
      'a'.parse <|> 'b'.parse | "aloha" toString,
      "a"
    )
    assertEquals(
      'a'.parse <|> 'b'.parse | "bloha" toString,
      "b"
    )
    assertEquals(
      'a'.parse <|> 'b'.parse | "abloha" toString,
      "a"
    )
  }

  @Test
  def TestAndThen: Unit = {
    assertEquals(
      'b'.parse >> 'c'.parse | "bcla-la-la" toString,
      "(b,c)"
    )
  }
}
