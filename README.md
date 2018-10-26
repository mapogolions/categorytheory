## sbt project compiled with Dotty

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).

## Contents

* Monomorphism
* Epimorphism
* Product
* Coproduct
* Functor
  * `fmap :: a -> b -> Maybe a    -> Maybe b`
  * `fmap :: a -> b -> List a     -> List b`
  * `fmap :: a -> b -> Reader r a -> Reader r b` where r - fixed parameter
  * `fmap :: a -> b -> Const c a  -> Const c b` - Black hole (`data Const c a = Const c`)
* Bifunctor - `Pair[A, B] = Pair(A, B)`
* BifunctorCo - `Either[A, B] = Left[A] | Right[B]`
* Functoriality:
  * `Pair _ b` ~ `Pair a _`
  * `Either _ b` ~ `Either a _`
