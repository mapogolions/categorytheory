package io.github.mapogolions.categorytheory


object Functor {
  type a = Int
  type b = Boolean
  type c = String

  val f: a => b = x => if (x == 0) false else true
  val g: b => c = x => if (x) "string" else ""
  def id[A](x: A): A = x

  type Fa = Maybe[a]
  type Fb = Maybe[b]
  type Fc = Maybe[c]

  /**
    * instance Functor `Maybe` where
    *   fmap :: a -> a -> Maybe a -> Maybe b
    */
  def fmapMaybe[A, B](f: A => B): Maybe[A] => Maybe[B] = {
    val g: Maybe[A] => Maybe[B] = _ match {
      case Nothing => Nothing
      case Just(x) => Just(f(x))
    }
    g
  }

  /**
    * instance Functor `List` where
    *   fmap :: a -> b -> List a -> List b
    */
  def fmapList[A, B](f: A => B): List[A] => List[B] = {
    val g: List[A] => List[B] = _ match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), fmapList(f)(t))
    }
    g
  }

  /**
    * instance Functor `Reader` where
    *   fmap :: a -> b -> Reader r a -> Reader r b
    * or
    *   fmap :: a -> b -> (r -> a) -> (r -> b) therefore, `fmap` for `Reader`
    *   fmap (a -> b) (r -> a) = (a -> b) . (r -> a)
    * `fmap` is just composition its arguments.
    * or
    *   fmap :: a -> b -> (-> r a) -> (-> r b)
    */
  def fmapReader[A, B, T](f: A => B): Reader[T, A] => Reader[T, B] = {
    val g: Reader[T, A] => Reader[T, B] = (h: Reader[T, A]) => new Reader[T, B] {
      override def apply(arg: T): B = (f compose h)(arg)
    }
    g
  }

  /**  Caution! Black hole :)
    * instance Functor `Const` where
    *   fmap :: a -> b -> Const c a -> Const c b
    *
    * fmap f = fmap g = fmap (g . f) = fmap id = id - singularity
    */
  def fmapConst[A, B, C](f: A => B): Const[C, A] => Const[C, B] = {
    val g: Const[C, A] => Const[C, B] = _ match {
      case Const(c) => Const[C, B](c)
    }
    g
  }

  /**
    * See the section `scala/test/FunctorTest.scala`
    * class `FunctorCompositionTest` for getting more
    * information about `functors composition`.
    * Type inference of Scala less powerful than type inference in haskell
    * therefore, fmap . fmap f G[F[_]] must be rewritten as
    *  fmap(fmap f)(G[F[_]])
    *
    */
}
