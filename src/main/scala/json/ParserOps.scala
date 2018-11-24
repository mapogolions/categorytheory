package io.github.mapogolions.json.ops

import io.github.mapogolions.json.adt._


object ParserOps {
  def parseLowerCase = Range('a', 'z').toList.map(_ toChar) anyOf
  def parseUpperCase = Range('A', 'Z').toList.map(_ toChar) anyOf
  def parseDigit = List('1', '2', '3', '4', '5', '6', '7', '8', '9') anyOf

  implicit class ListOfCharsOpts(ls: List[Char]) {
    def anyOf = ls.map(_ parse).choice
  }
 
  implicit class ListOfParserOps[A, B](ls: List[Parser[A]]) {
    def choice: Parser[A] = ls.reduce(_ <|> _)
  }
  implicit class CharOps(ch: Char) {
    def parse: Parser[Char] = new Parser[Char] {
      def apply(token: String): Result[Char] =
        if (!token.nonEmpty) Failure("No more")
        else if (token(0) == ch) Success(Cons(ch, Nil), token.slice(1, token.length))
        else Failure(s"Expecting ${ch}. Got ${token(0)}")
    }
  }
}