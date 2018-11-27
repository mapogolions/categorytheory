package io.github.mapogolions.json.adt

import io.github.mapogolions.json.functor._


trait Result[+A]
case class Success[A](val elem: A, msg: String) extends Result[A]
case class Failure(val err: String) extends Result[Nothing]

trait Parser[A] { self =>
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
