package io.github.mapogolions.cats.bifunctor

import io.github.mapogolions.cats.adt._
import io.github.mapogolions.cats.functor.Functor
/*
fmap :: (a -> b) -> (fa -> fb)

bimap :: (a -> c) -> (b -> d) -> (fab -> fcd)
 */
trait Bifunctor[G[_, _]] { self =>
  /* maps */
  def bimap[A, C, B, D](fab: G[A, B])(f: A => C, g: B => D): G[C, D]
  def leftMap[A, C, B](fab: G[A, B])(f: A => C): G[C, B] = bimap(fab)(f, identity)
  def rightMap[A, B, D](fab: G[A, B])(g: B => D): G[A, D] = bimap(fab)(identity, g)
  def umap[A, B, C](faa: G[A, B])(f: A | B => C): G[C, C] = bimap(faa)(f, f)

  /* lifts */
  def lift[A, C, B, D](f: A => C, g: B => D): G[A, B] => G[C, D]
  def first[A, C, B](f: A => C): G[A, B] => G[C, B] = lift(f, identity)
  def second[A, B, D](g: B => D): G[A, B] => G[A, D] = lift(identity, g)
  /* How to use: val FR = Bifunctor[Pair].rightFunctor[Char]
                 FR.map(Pair('a', 10))(_ > 0)
  */
  def rightFunctor[A]: Functor[[X] => G[A, X]] = new Functor[[X] => G[A, X]] {
    override def map[B, D](fab: G[A, B])(f: B => D): G[A, D] =
      self.bimap(fab)(identity, f)
  }

  def leftFunctor[B]: Functor[[X] => G[X, B]] = new Functor[[X] => G[X, B]] {
    override def map[A, C](fab: G[A, B])(f: A => C): G[C, B] =
      self.bimap(fab)(f, identity)
  }
}
// Bifunctor[List] map
object Bifunctor {
  def apply[G[_, _]: Bifunctor] = implicitly
  // def apply[G[_, _]](implicit bifunctorInstance: Bifunctor[G]) = bifunctorInstance
}

object BifunctorSyntax {
  implicit class BifunctorOps[G[_, _]: Bifunctor, A, B](G: G[A, B]) {
    def umap[C](f: A | B => C): G[C, C] = implicitly.umap(G)(f)
    def leftMap[C](f: A => C): G[C, B] = implicitly.leftMap(G)(f)
    def rightMap[D](g: B => D): G[A, D] = implicitly.rightMap(G)(g)
    def bimap[C, D](f: A => C, g: B => D): G[C, D] = implicitly.bimap(G)(f, g)

    def lift[C, D](f: A => C, g: B => D): () => G[C, D] = () => implicitly.lift(f, g)(G)
    def first[C](f: A => C): () => G[C, B] = () => implicitly.first(f)(G)
    def second[D](g: B => D): () => G[A, D] = () => implicitly.second(g)(G)
  }
}

object BifunctorInstances {
  implicit val eitherBifunctor: Bifunctor[Either] = new Bifunctor[Either] {
    def bimap[A, C, B, D](fab: Either[A, B])(f: A => C, g: B => D): Either[C, D] =
      fab match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }

    def lift[A, C, B, D](f: A => C, g: B => D): Either[A, B] => Either[C, D] = {
      val h: Either[A, B] => Either[C, D] = bimap(_)(f, g)
      h
    }
  }

  implicit val rightBifunctor: Bifunctor[Right] = new Bifunctor[Right] {
    def bimap[A, C, B, D](fab: Right[A, B])(f: A => C, g: B => D): Right[C, D] =
      fab match {
        case Right(b) => Right(g(b))
      }

    def lift[A, C, B, D](f: A => C, g: B => D): Right[A, B] => Right[C, D] = {
      val h: Right[A, B] => Right[C, D] = bimap(_)(f, g)
      h
    }
  }

  implicit val leftBifunctor: Bifunctor[Left] = new Bifunctor[Left] {
    def bimap[A, C, B, D](fab: Left[A, B])(f: A => C, g: B => D): Left[C, D] =
      fab match {
        case Left(a) => Left(f(a))
      }

    def lift[A, C, B, D](f: A => C, g: B => D): Left[A, B] => Left[C, D] = {
      val h: Left[A, B] => Left[C, D] = bimap(_)(f, g)
      h
    }
  }

  implicit val pairBifunctor: Bifunctor[Pair] = new Bifunctor[Pair] {
    def bimap[A, C, B, D](fab: Pair[A, B])(f: A => C, g: B => D): Pair[C, D] =
      fab match {
        case Pair(fst, snd) => Pair(f(fst), g(snd))
      }

    def lift[A, C, B, D](f: A => C, g: B => D): Pair[A, B] => Pair[C, D] = {
      val h: Pair[A, B] => Pair[C, D] = bimap(_)(f, g)
      h
    }
  }
}
