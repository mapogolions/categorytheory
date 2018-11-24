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
case class Success[A](val elem: Cons[A], msg: String) extends Result[A]
case class Failure(val err: String) extends Result[Nothing]

trait Parser[A] { self => 
  def apply(token: String): Result[A]

  def |>[B](f: A => B): Parser[B] = new Parser {
    def apply(token: String): Result[B] = (self apply token) match {
      case Success(h, t) => Success(h.map(f), t)
      case Failure(err) => Failure(err)
    }
  }

  def >>(pb: Parser[A]): Parser[A] = new Parser[A] {
    def apply(token: String): Result[A] = (self apply token) match {
      case Failure(e)      => Failure(e)
      case Success(h1: Cons[A], t1) => (pb apply t1) match {
        case Failure(e)      => Failure(e)
        case Success(h2: Cons[A], t2) => 
          Success(LinkedList.appendCons(h1, h2), t2)
      }
    }
  }

  def <|>(pb: Parser[A]): Parser[A] = new Parser[A] {
    def apply(token: String): Result[A] = (self apply token) match {
      case Success(h: A, t) => Success(Cons(h, Nil), t)
      case Failure(e)    => pb.apply(token)
    }
  }
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