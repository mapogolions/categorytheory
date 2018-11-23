package io.github.mapogolions.cats.adt
import io.github.mapogolions.cats.functor.Functor
import io.github.mapogolions.cats.functor.FunctorInstances._
import io.github.mapogolions.cats.functor.FunctorSyntax._
// Types for json parsing lib
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
      case Success(h1: A, t1) => (pb apply t1) match {
        case Failure(e)      => Failure(e)
        case Success(h2: A, t2) => Success(Cons(h1, Cons(h2, Nil)), t2)
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
// case class Parser[A](val f: String => Result[A])


trait Reader[R, A] extends Function[R, A]

/* data (, ) */
case class Pair[A, B](val first: A, val second: B)

trait Either[+A, +B]
case class Left[A, ?](val left: A) extends Either[A, Nothing]
case class Right[?, B](val right: B) extends Either[Nothing, B]

/* data Mabe a = Just a | Blank */
trait Maybe[+A]
case object Blank extends Maybe[Nothing]
case class Just[A](val field: A) extends Maybe[A]

/* data List a = Cons a List a | Nil */
trait List[+A]

case object Nil extends List[Nothing]
case class Cons[A](val head: A, val tail: List[A]) extends List[A]

object List {
  def apply[A](items: A*): List[A] = {
    if (items.length == 0) Nil
    else Cons(items.head, apply(items.tail: _*))
  }

  def append[A](as: List[A], bs: List[A]): List[A] = {
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
