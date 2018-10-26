package io.github.mapogolions.cats

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo

import io.github.mapogolions.cats.functor.Functor
import io.github.mapogolions.cats.adt.{
  List, Cons, Nil, Maybe, Just, Blank, Pair, Either, Left, Right
}


class TestFunctor {
  import io.github.mapogolions.cats.functor.FunctorIntances._
  import io.github.mapogolions.cats.functor.FunctorSyntax._

  @Test
  def functorComposition(): Unit = {
    val listWithMaybe = Functor[List] compose Functor[Maybe]
    assertEquals(
      listWithMaybe.map(List(Just(10), Just(10), Just(-1)))( _ + 1 ),
      List(Just(11), Just(11), Just(0))
    )

    assertEquals(
      listWithMaybe.as(List(Just(10), Blank, Just(-1)), '~'),
      List(Just('~'), Blank, Just('~'))
    )

    assertEquals(
      listWithMaybe.void(List(Just(10), Blank, Just(-1))),
      List(Just(()), Blank, Just(()))
    )
  }

  @Test
  def liftTest(): Unit = {
    assertEquals(
      Functor[List].lift[Char, String](_.toString)(List('z')),
      List("z")
    )
    assertEquals(
      Functor[Maybe].lift[Int, Boolean](_ < 0)(Just(10)),
      Just(false)
    )
    assertEquals(
      (Just("hello") lift { _.contains('!') })(),
      Just(false)
    )
    assertEquals(
      (Nil lift identity)(),
      Nil
    )
    assertEquals(
      (List(0, 3, -10) lift { _ > 0 })(),
      List(false, true, false)
    )
  }

  @Test
  def asTest(): Unit = {
    assertEquals(
      Functor[List].as(List(1, 2, 3), ""),
      List("", "", "")
    )
    assertEquals(Just(10) as Just('!'), Just(Just('!')))
    assertEquals(Blank as false, Blank)

    assertEquals(
      List(1, 2, 4) as 3, List(3, 3, 3)
    )
    assertEquals(Nil as "black hole", Nil)
    assertEquals(
      Cons('y', Cons('e', Cons('s', Nil))) as "cat",
      Cons("cat", Cons("cat", Cons("cat", Nil)))
    )
  }

  @Test
  def voidTest(): Unit = {
    assertEquals(
      Functor[List].void(List(1, 2, 4)),
      Cons((), Cons((), Cons((), Nil)))
    )
    assertEquals(
      Functor[Maybe].void(Blank),
      Blank
    )
    assertEquals(Just(10) void, Just(()))
    assertEquals(Blank void, Blank)
    assertEquals(Just("hello") void , Just(()))

    assertEquals(
      List(1, 2, 3).void,
      List((), (), ())
    )
    assertEquals(
      Cons("one", Cons("two", Nil)) void,
      Cons((), Cons((), Nil))
    )
  }

  @Test
  def mapTest(): Unit = {
    assertEquals(
      Functor[List].map(Nil: List[Char])( _ < 'z'),
      Nil
    )
    assertEquals(
      Functor[Maybe].map(Blank: Maybe[Int])(_ < 0),
      Blank
    )
    assertEquals(
      Functor[List].map(List(0, -2, 10))(_ < 0),
      List(false, true, false)
    )
    assertEquals(
      Functor[Maybe].map(Just(0))(_ > 0),
      Just(false)
    )
    // `Blank`, `Just` map
    assertEquals(
      Just('!') map { "hello" + _ },
      Just("hello!")
    )
    assertEquals(Blank map identity, Blank)
    assertEquals(Just(10) map { _ < 0 }, Just(false))

    // `List`, `Cons`, `Nil` map
    assertEquals(
      List(1, 2) map { _ > 0 }, List(true, true)
    )
    assertEquals(
      List(0, -1) map { _ < 0 }, List(false, true)
    )
    assertEquals(
      Cons('a', Cons('b', Nil)) map { _ < 'z' },
      Cons(true, Cons(true, Nil))
    )
    assertEquals(
      List("i", "love", "this", "game") map { _.length > 2 },
      List(false, true, true, true)
    )
  }
}
