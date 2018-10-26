package io.github.mapogolions.categorytheory


// data Pair a b = Pair a b
case class Pair[A, B](val fst: A, val snd: B)

// data Either a b = Left a | Right b
trait Either[+A, +B]
case class Left[A](val field: A) extends Either[A, Nothing]
case class Right[B](val field: B) extends Either[Nothing, B]

// data Const c a = Const c (Image of black hole - singularity)
case class Const[C, ?](val field: C)

// data Maybe a = Nothing | Just a
trait Maybe[+A]
case object Nothing extends Maybe[Nothing]
case class Just[+A](field: A) extends Maybe[A]

// data List a  = Nil | Cons a
trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](val head: A, val tail: List[A]) extends List[A]

// data Reader r a = r -> a
trait Reader[R, A] extends Function[R, A]
