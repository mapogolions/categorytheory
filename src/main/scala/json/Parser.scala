package io.github.mapogolions.json.adt

import io.github.mapogolions.json.adt.{ Result, Success, Failure }
import io.github.mapogolions.json.functor.Functor
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._
import io.github.mapogolions.json.applicative.Applicative
import io.github.mapogolions.json.applicative.ApplicativeInstances._
import io.github.mapogolions.json.applicative.ApplicativeSyntax._
import io.github.mapogolions.json.monad.Monad
import io.github.mapogolions.json.monad.MonadInstances._
import io.github.mapogolions.json.monad.MonadSyntax._


trait Parser[A](val label: String="unknow") { self =>
  // Parses one or more occurrences of p separated by sep
  def sep[B](pb: Parser[B]) = 
    self >> (pb |> self many) map { (x, xs) => x :: xs }
  // Keep only result of the middle parser - between
  def between[B, C](pb: Parser[B])(pc: Parser[C]) = pb |> self <| pc

  // throwing results away
  def <|[B](pb: Parser[B]) = self >> pb map { (a, b) => a }
  def |>[B](pb: Parser[B]) = self >> pb map { (a, b) => b }

  // matching parser zero or one time
  def opt: Parser[Option[A]] = 
      self.map(Some(_)) <|> (Applicative[Parser] pure None)

  def once = self

  
  def many: Parser[List[A]] = new Parser[List[A]]("many") {
    def apply(source: Source): Result[List[A]] = {
      val (ls, rest) = self anytimes source
      Success(ls, rest)
    }
  }

  def atLeastOne: Parser[List[A]] = new Parser[List[A]]("atLeastOne") {
    def apply(source: Source) = self oneOrMore source
  }

  def times(n: Int): Parser[List[A]] = new Parser[List[A]]("repeat times") {
    def apply(source: Source) = 
      if (n <= 0) Failure(label, s"Unexpected $n", Position from source)
      else self.ntimes(source, n)
  }

  private def ntimes(source: Source, n: Int): Result[List[A]] = {
    @annotation.tailrec
    def loop(src: Source, acc: List[A], count: Int): Result[List[A]] = {
      (count, self apply src) match {
        case (0, _) => Success(acc.reverse, src)
        case (_, Failure(label, err, pos)) => Failure(label, err, pos)
        case (_, Success(h, t)) => loop(t, h :: acc, count - 1)
      }
    }
    loop(source, Nil, n)
  }

  private def oneOrMore(source: Source): Result[List[A]] =
    (self apply source) match {
      case Failure(label, err, pos) => Failure(label, err, pos)
      case Success(h, t) => {
        val (ch, src) = self anytimes t
        Success(h :: ch,  src)
      }
    }

  private def anytimes(source: Source): (List[A], Source) =
    (self apply source) match {
      case Failure(_, _, _) => (Nil, source)
      case Success(h, t) => {
         val (ch, src) = self anytimes t
        (h :: ch, src)
      }
    }

  def >>[B](pb: Parser[B]): Parser[(A, B)] = new Parser[(A, B)]() {
    def apply(source: Source): Result[(A, B)] = (self apply source) match {
      case Failure(label1, err1, pos1) => Failure(label1, err1, pos1)
      case Success(h1, t1) => (pb apply t1) match {
        case Failure(label2, err2, pos2) => Failure(label2, err2, pos2)
        case Success(h2, t2) => Success((h1, h2), t2)
      }
    }
  } ?? s"${self.label} andThen ${pb.label}"

  def ??(label: String): Parser[A] = new Parser[A](label) {
    def apply(source: Source): Result[A] = (self apply source) match {
      case Success(h, t) => Success(h, t)
      case Failure(_, err, pos) => Failure(label, err, pos)
    }
  }

  def <|>[B](pb: Parser[B]): Parser[A | B] = new Parser[A | B]() {
    def apply(source: Source): Result[A | B] = (self apply source) match {
      case Success(h, t) => Success(h, t)
      case Failure(_, _, _) => (pb apply source)
    }
  } ?? s"${self.label} orElse ${pb.label}"

  def apply(source: Source): Result[A]
  def | (text: String) = apply (Source from text)
}
