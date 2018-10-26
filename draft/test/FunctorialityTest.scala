import io.github.mapogolions.categorytheory._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo


class FunctorialityTest {
  @Test
  def functorialityPairTest(): Unit = {
    /**
      * bimap (a -> c) -> (b -> d) -> Fab -> Fcd
      * Let's forget about one of the type parameters (fixed one of them temporarily)
      * first (a -> c) -> Fab -> Fcb  ~ It's like `Functor` which acts on the first argument
      * second (b -> d) -> Fab -> Fad ~ It's like `Functor` which acts on the second argument
      *
      */
    // We are fixed first parameter, and it works like `Functor` on second parameter
    def fmap1[A, B](f: A => B): Pair[_, A] => Pair[_, B] = {
      val g: Pair[_, A] => Pair[_, B] = _ match {
        case Pair(a, b) => Pair(a, f(b))
      }
      g
    }
    // We are fixed second parameter, and it works like `Funtor` on the first parameter
    def fmap2[A, B](f: A => B): Pair[A, _] => Pair[B, _] = {
      val g: Pair[A, _] => Pair[B, _] = _ match {
        case Pair(a, b) => Pair(f(a), b)
      }
      g
    }
    assertEquals(
      fmap1(Functor.f)( Pair('x', 2) ),
      Bifunctor.bisecondPair(Functor.f)( Pair('x', 2) )
    )
    assertEquals(
      fmap2(Functor.f)( Pair(0, 'x') ),
      Bifunctor.bifirstPair(Functor.f)( Pair(0, 'x') )
    )
  }

  @Test
  def functorialityEitherTest(): Unit = {
    // We are fixed first parameter, and it works like `Functor` on second parameter
    def fmap1[A, B](f: A => B): Either[_, A] => Either[_, B] = {
      val g: Either[_, A] => Either[_, B] = _ match {
        case Right(b) => Right(f(b))
        case Left(a) => Left(a)
      }
      g
    }
    // We are fixed second parameter, and it works like `Funtor` on the first parameter
    def fmap2[A, B](f: A => B): Either[A, _] => Either[B, _] = {
      val g: Either[A, _] => Either[B, _] = _ match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(b)
      }
      g
    }

    assertEquals(
      fmap2(Functor.f)(Left(10)),
      Bifunctor.bifirstEither(Functor.f)(Left(10))
    )
    assertEquals(
      fmap2(Functor.f)(Right(10)),
      Bifunctor.bifirstEither(Functor.f)(Right(10))
    )
    assertEquals(
      fmap1(Functor.f)(Right(10)),
      Bifunctor.bisecondEither(Functor.f)(Right(10))
    )
  }
}
