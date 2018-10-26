package io.github.mapogolions.cats.monad

import io.github.mapogolions.cats.adt._

/* 
>=> :: (a -> mb) -> (b -> mc) -> (a -> mc)
>=> f g = \a -> let mb = (f a)
                     in (mb =>> g)

=>> :: mb -> (b -> mc) -> mc

 */

trait Monad[M[_]] {
  def >=>[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a: A => ->>(f(a))(g)
  def ->>[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object MonadSyntax {
  implicit class MonadOps[M[_], A](M: M[A])(implicit monadInstance: Monad[M]) {
    def ->>[B](f: A => M[B]): M[B] = monadInstance.->>(M)(f)
  }

  implicit class FishOperatorOps[M[_], A, B](B: A => M[B])(implicit monadInstance: Monad[M]) {
    def >=>[C](g: B => M[C]): A => M[C] = monadInstance.>=>(B, g)
  }
}

object MonadInstances {
  implicit val justMonad: Monad[Just] = new Monad[Just] {
    def ->>[A, B](ma: Just[A])(f: A => Just[B]): Just[B] = ma match {
      case Just(a) => f(a)
    }
  }

  implicit val consMonad: Monad[Cons] = new Monad[Cons] {
    def ->>[A, B](ma: Cons[A])(f: A => Cons[B]): Cons[B] = ma match {
      case Cons(h, Nil) => f(h)
      case Cons(h, t: Cons[A]) => List.appendCons(f(h), ->>(t)(f))
    }
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    def ->>[A, B](ma: List[A])(f: A => List[B]): List[B] = ma match {
      case Nil        => Nil
      case Cons(h, t) => List.append(f(h), ->>(t)(f))
    }
  }
  implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {
    def ->>[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] = ma match {
      case Blank   => Blank
      case Just(a) => f(a)
    }
  }
}
