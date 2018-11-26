package io.github.mapogolions.json.ops

import io.github.mapogolions.json.adt._
import io.github.mapogolions.json.applicative.Applicative
import io.github.mapogolions.json.applicative.ApplicativeInstances._
import io.github.mapogolions.json.applicative.ApplicativeSyntax._
import io.github.mapogolions.json.functor.Functor
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._


object ParserOps {
  def anytimes[A](pa: Parser[A], token: String): (List[A], String) = {
    (pa apply token) match {
      case Failure(e) => (Nil, token)
      case Success(h, t) => {
        val res = anytimes(pa, t)
        (h :: res._1, res._2)
      }
    }
  }
  def sequence[A](ls: List[Parser[A]]): Parser[List[A]] = {
    def cons[A] =  lift({ x: A => xs: List[A] => x :: xs })
    ls match {
      case Nil => Applicative[Parser].pure(Nil)
      case (x::xs) => cons(x)(sequence(xs))
    }
  }
  def startWith = lift({ token: String => token.startsWith })
  def add = lift({ a: Int => b: Int => a + b })
  def lift[A, B, C](f: A => B => C)(pa: Parser[A])(pb: Parser[B]) =
    Applicative[Parser].pure(f).ap(pa).ap(pb)

  def parserThreeDigitsAsInt = parseThreeDigitsAsStr map { _.toInt }
  def parseThreeDigitsAsStr = {
    def transform(xs: ((Char, Char), Char)): String = 
      xs match { case ((a, b), c) => s"${a}${b}${c}" }
    parseThreeDigits.map(transform)
  }
  def parseThreeDigits = parseDigit >> parseDigit >> parseDigit
  def parseLowerCase = Range('a', 'z').toList.map(_ toChar) anyOf
  def parseUpperCase = Range('A', 'Z').toList.map(_ toChar) anyOf
  def parseDigit = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9') anyOf
  
  implicit class StringOps(str: String) {
    def parse: Parser[String] = 
      sequence(str.toList.map(_ parse)) map { _ mkString("") }
  }

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
        else if (token(0) == ch) Success(ch, token.slice(1, token.length))
        else Failure(s"Expecting ${ch}. Got ${token(0)}")
    }
  }
}