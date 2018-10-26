package io.github.mapogolions.categorytheory


object Functoriality {
  /**
    * Functoriality is meaning that if we have `type constructor` with at least
    * 2 parameter (for instance Pair a b) and one of them fixed temporarily, then
    * we must have able to write (express) it as `Functor` on another
    * (free) type parameter.
    *
    * Canonical definition `bimap` and `fmap`
    *   bimap :: (a -> c) -> (b -> d) -> Fab -> Fcd
    *
    * But what if we will fix one of the type parameter
    *   first :: (a -> c) -> Fab -> Fcb
    *   first f (Pair a b) = Pair (f a) b
    *   bimap f _ (Pair a b) = Pair (f a) b
    *   fmap :: (a -> c) -> Fab -> Fcb
    *   fmap f (Pair a b) = Pair (f a) b
    * Therefore `first` ~ `bimap f _` ~ `fmap`
    * or
    *   second ::(b -> d) -> Fab -> Fad
    *   second g (Pair a b) = Pair a (g b)
    *   bimap _ g (Pair a b) = Pair a (g b)
    *   fmap :: (b -> d) -> Fab -> Fad
    *   fmap g (Pair a b) = Pair a (g b)
    * Therefore `second` ~ `bimap _ g` ~ `fmap`
    *
    * See the section `scala/test/FunctorialityTest.scala` for getting more
    * information about `functoriality proof` for different type constructors.
    */
}
