package io.github.mapogolions.cats.adt


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
