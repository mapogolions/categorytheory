package io.github.mapogolions.cats.functor

import io.github.mapogolions.cats.adt._


trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
  def void[A](fa: F[A]): F[Unit] = as(fa, ())
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))
  // Functor[Just] compose Functor[List] -> [X] -> 
  def compose[G[_]: Functor]: Functor[[X] => F[G[X]]] =
    new Functor[[X] => F[G[X]]] {
      override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
        self.map(fga)(fa => implicitly.map(fa)(f))
        // self.map(fga)(fa => Functor[G].map(fa)(f))
    }
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly
  // def apply[F[_]](implicit functorInstance: Functor[F]): Functor[F] = functorInstance
}

object FunctorSyntax {
  implicit class FunctorOps[F[_]: Functor, A](F: F[A]) {
    def map[B](f: A => B): F[B] = implicitly.map(F)(f)
    def lift[B](f: A => B): () => F[B] = () => implicitly.lift(f)(F)
    def as[B](b: B): F[B] = implicitly.as(F, b)
    def void[B]: F[Unit] = implicitly.void(F)
    def fproduct[B](f: A => B): F[(A, B)] = implicitly.fproduct(F)(f)
  }
}

object FunctorInstances {
  implicit val consFunctor: Functor[Cons] = new Functor[Cons] {
    def map[A, B](fa: Cons[A])(f: A => B): Cons[B] =
      fa match {
        case Cons(h, Nil) => Cons(f(h), Nil)
        case Cons(h, t: Cons[A]) => Cons(f(h), map(t)(f))
      }
  }

  implicit val justFunctor: Functor[Just] = new Functor[Just] {
    def map[A, B](fa: Just[A])(f: A => B): Just[B] =
      fa match {
        case Just(a) => Just(f(a))
      }
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case Nil => Nil
        case Cons(h, t) => Cons(f(h), map(t)(f))
      }
  }

  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] =
      fa match {
        case Blank => Blank
        case Just(a) => Just(f(a))
      }
  }
}
