package io.github.mapogolions.json.adt

import io.github.mapogolions.json.functor.Functor
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._
import io.github.mapogolions.json.applicative.Applicative
import io.github.mapogolions.json.applicative.ApplicativeInstances._
import io.github.mapogolions.json.applicative.ApplicativeSyntax._


trait Result[+A]
case class Success[A](val elem: A, msg: String) extends Result[A]
case class Failure(val err: String) extends Result[Nothing]

trait Parser[A] { self =>
  // Keep only result of the middle parser - between
  def between[B, C](pb: Parser[B])(pc: Parser[C]) = pb |> self <| pc

  // throwing results away
  def <|[B](pb: Parser[B]) = self >> pb map { (a, b) => a }
  def |>[B](pb: Parser[B]) = self >> pb map { (a, b) => b }

  // matching parser zero or one time
  def opt: Parser[Option[A]] = 
      self.map(Some(_)) <|> (Applicative[Parser] pure None)

  def once = self

  def many: Parser[List[A]] = new Parser[List[A]] {
    def apply(token: String) = {
      val (ls, rest) = self anytimes token
      Success(ls, rest)
    }
  }

  def atLeastOne: Parser[List[A]] = new Parser[List[A]] {
    def apply(token: String) = self oneOrMore token
  }
  
  private def oneOrMore(token: String): Result[List[A]] = {
    (self apply token) match {
      case Failure(e) => Failure(e)
      case Success(h, t) => {
        val (ls, rest) = self anytimes t
        Success(h::ls,  rest)
      }
    }
  }

  private def anytimes(token: String): (List[A], String) = {
    (self apply token) match {
      case Failure(e) => (Nil, token)
      case Success(h, t) => {
        val res = self anytimes t
        (h :: res._1, res._2)
      }
    }
  }

  def >>[B](pb: Parser[B]): Parser[(A, B)] = new Parser[(A, B)] {
    def apply(token: String): Result[(A, B)] = (self apply token) match {
      case Failure(e) => Failure(e)
      case Success(h1, t1) => (pb apply t1) match {
        case Failure(e) => Failure(e)
        case Success(h2, t2) => Success((h1, h2), t2)
      }
    }
  }

  def <|>[B](pb: Parser[B]): Parser[A | B] = new Parser[A | B] {
    def apply(token: String): Result[A | B] = (self apply token) match {
      case Success(h, t) => Success(h, t)
      case Failure(e) => (pb apply token)
    }
  }

  def apply(token: String): Result[A]
  def | = apply
}
