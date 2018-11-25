package io.github.mapogolions.json.adt

import io.github.mapogolions.json.functor.Functor
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._

trait LinkedList[+A]
case object Nil extends LinkedList[Nothing]
case class Cons[A](
  val head: A, 
  val tail: LinkedList[A]
) extends LinkedList[A]


trait Result[+A]
//case class Success[A](val elem: Cons[A], msg: String) extends Result[A]
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
}

object LinkedList {
  def apply[A](items: A*): LinkedList[A] = {
    if (items.length == 0) Nil
    else Cons(items.head, apply(items.tail: _*))
  }

  def append[A](as: LinkedList[A], bs: LinkedList[A]): LinkedList[A] = {
    (as, bs) match {
      case (Nil, _) => bs
      case (_, Nil) => as
      case (Cons(h, t), Cons(_, _)) => Cons(h, append(t, bs))
    }
  }
  def appendCons[A](as: Cons[A], bs: Cons[A]): Cons[A] = {
    (as, bs) match {
      case (Nil, _) => bs
      case (Cons(h, t), Cons(_, _)) => Cons(h, append(t, bs))
    }
  }
}