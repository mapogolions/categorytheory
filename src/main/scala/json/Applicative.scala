package io.github.mapogolions.json.applicative

import io.github.mapogolions.json.adt._
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._

trait Applicative[F[_]] {
  def pure[A](x: A): F[A]
  def unit: F[Unit] = pure(())
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  def apply[F[_]: Applicative] = implicitly
}

object ApplicativeSyntax {
  implicit class FunctorOps[F[_]: Applicative, A](F: F[A]) {
    def pure(x: A): F[A] = implicitly.pure(x)
    def unit: F[Unit] = implicitly.unit
    def ap[B](ff: F[A => B]): F[B] = implicitly.ap(ff)(F)
  }
}

object ApplicativeInstances {
  import io.github.mapogolions.json.functor.FunctorSyntax._
  import io.github.mapogolions.json.functor.FunctorInstances._
  
  val parserApplicative: Applicative[Parser] = new Applicative[Parser] {
    def pure[A](x: A): Parser[A] = new Parser[A] { self =>
      def apply(token: String): Result[A] = (self apply token) match {
        case Success(h, t) => Success(x, t)
        case Failure(e) => Failure(e)
      }
    }
    /**
     * f :: a -> b
     * pf :: (f -> rf) ~ (a -> b) -> r(a -> b)
     * pa :: (a -> ra)
     * (pf >> pa) :: p((f, a)) ~ p(((a -> b), a))
     * p((f, a)).map((f, a) => f(a))
     *
    */
    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      (ff >> fa).map((f, a) => f(a))
  }
}