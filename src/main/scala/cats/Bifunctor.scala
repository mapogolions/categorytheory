package io.github.mapogolions.cats.bifunctor

import io.github.mapogolions.cats.adt._
import io.github.mapogolions.cats.functor.Functor
/*
fmap :: (a -> b) -> (fa -> fb)

bimap :: (a -> c) -> (b -> d) -> (fab -> fcd)
 */
trait Bifunctor[G[_, _]] {
  /* maps */
  def bimap[A, C, B, D](fab: G[A, B])(f: A => C, g: B => D): G[C, D]
  def leftMap[A, C, B](fab: G[A, B])(f: A => C): G[C, B] = bimap(fab)(f, identity)
  def rightMap[A, B, D](fab: G[A, B])(g: B => D): G[A, D] = bimap(fab)(identity, g)
  def umap[A, B, C](faa: G[A, B])(f: A | B => C): G[C, C] = bimap(faa)(f, f)

  /* lifts */
  def lift[A, C, B, D](f: A => C, g: B => D): G[A, B] => G[C, D]
  def first[A, C, B](f: A => C): G[A, B] => G[C, B] = lift(f, identity)
  def second[A, B, D](g: B => D): G[A, B] => G[A, D] = lift(identity, g)
  /* extract functors */
  // Pair(10, 'a') leftFunctor  -> Pair[Int, ?](10)
  // fab = Pair(10, false)  f
}
// Bifunctor[List] map
object Bifunctor {
  def apply[G[_, _]](implicit bifunctorInstance: Bifunctor[G]) = bifunctorInstance
}

object BifunctorSyntax {
  implicit class BifunctorOps[G[_, _], A, B](G: G[A, B])(implicit bifunctorInstance: Bifunctor[G]) {
    def umap[C](f: A | B => C): G[C, C] = bifunctorInstance.umap(G)(f)
    def leftMap[C](f: A => C): G[C, B] = bifunctorInstance.leftMap(G)(f)
    def rightMap[D](g: B => D): G[A, D] = bifunctorInstance.rightMap(G)(g)
    def bimap[C, D](f: A => C, g: B => D): G[C, D] = bifunctorInstance.bimap(G)(f, g)

    def lift[C, D](f: A => C, g: B => D): () => G[C, D] = () => bifunctorInstance.lift(f, g)(G)
    def first[C](f: A => C): () => G[C, B] = () => bifunctorInstance.first(f)(G)
    def second[D](g: B => D): () => G[A, D] = () => bifunctorInstance.second(g)(G)
  }
}

object BifunctorInstances {
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
