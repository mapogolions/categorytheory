package io.github.mapogolions.json.monad

import io.github.mapogolions.json.adt._


trait Monad[M[_]] {
  def >=>[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = 
    a: A => >>=(f(a))(g)
  def >>=[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object MonadSyntax {
  implicit class BindOperatorOps[M[_]: Monad, A](M: M[A]) {
    def >>=[B](f: A => M[B]): M[B] = implicitly.>>=(M)(f)
  }

  implicit class FishOperatorOps[M[_]: Monad, A, B](B: A => M[B]) {
    def >=>[C](g: B => M[C]): A => M[C] = implicitly.>=>(B, g)
  }
}

object MonadInstances {
  implicit val parserMonad: Monad[Parser] = new Monad[Parser] {
    def >>=[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
      new Parser[B]() {
        def apply(state: State): Result[B] = (fa apply state) match {
          case Failure(label, err, pos) => Failure(label, err, pos)
          case Success(h, t) => f(h) apply t
        }
      }
  }
}
