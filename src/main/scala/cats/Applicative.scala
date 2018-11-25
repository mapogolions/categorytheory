package io.github.mapogolions.cats.applicative

import io.github.mapogolions.cats.adt._

trait Applicative[F[_]] {
  def pure[A](x: A): F[A]
  def unit: F[Unit] = pure(())
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)
}

object Applicative {
  def apply[F[_]: Applicative] = implicitly
}

object ApplicativeSyntax {
  implicit class ApplicativeOps[F[_]: Applicative, A](F: F[A]) {
    // def pure(x: A): F[A] = implicitly.pure(x)
    def map[B](f: A => B): F[B] = implicitly.map(F)(f)
    def unit: F[Unit] = implicitly.unit
    def ap[B](ff: F[A => B]): F[B] = implicitly.ap(ff)(F)
  }
}

object ApplicativeInstances {
  import io.github.mapogolions.cats.functor.FunctorSyntax._
  import io.github.mapogolions.cats.functor.FunctorInstances._

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](x: A): List[A] = List(x)
    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      (ff, fa) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(f, _), _) => fa.map(f)
      }
  }

  implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
    def pure[A](x: A): Maybe[A] = Just(x)
    def ap[A, B](ff: Maybe[A => B])(fa: Maybe[A]): Maybe[B] =
      (ff, fa) match {
        case (Blank, _) => Blank
        case (_, Blank) => Blank
        case (Just(f), _) => fa.map(f)
      }
  }
}