import io.github.mapogolions.categorytheory._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo



class FunctorCompositionTest {
  @Test
  def composition(): Unit = {
    // fmap . fmap f G[F[_]]
    // Maybe[List[_]]
    assertEquals(
      Functor.fmapMaybe(Functor.fmapList(Functor.f))(Just(Cons(2, Cons(0, Nil)))),
      Just(Cons(true, Cons(false, Nil)))
    )

    // List[Maybe[_]]
    assertEquals(
      Functor.fmapList(Functor.fmapMaybe(Functor.f))(Cons(Just(10), Cons(Just(0), Nil))),
      Cons(Just(true), Cons(Just(false), Nil))
    )
  }
}

class FunctorListTest {
  @Test
  def preserveIdentityTest(): Unit = {
    // fmap id = id
    assertEquals(
      Functor.fmapList(Functor.id)(Cons(true, Cons(false, Nil))),
      Functor.id(Cons(true, Cons(false, Nil)))
  )
    assertEquals( Functor.fmapList(Functor.id)(Nil), Functor.id(Nil) )
  }

  @Test
  def preserveCompositionTest(): Unit = {
    // fmap (g . f) = fmap g . fmap f
    assertEquals(
      Functor.fmapList(Functor.g compose Functor.f)(Cons(10, Cons(0, Nil))),
      (Functor.fmapList(Functor.g) compose Functor.fmapList(Functor.f))(Cons(10, Cons(0, Nil)))
    )
    assertEquals(
      Functor.fmapList(Functor.id compose Functor.f)(Nil),
      (Functor.fmapList(Functor.id) compose Functor.fmapList(Functor.f))(Nil)
    )
  }
}

class FunctorMaybeTest {
  @Test
  def preserveIdentityTest(): Unit = {
    // fmap id = id
    assertEquals( Functor.fmapMaybe(Functor.id)(Just('x')), Functor.id(Just('x')) )
    assertEquals( Functor.fmapMaybe(Functor.id)(Nothing), Functor.id(Nothing) )
  }

  @Test
  def preserveCompositionTest(): Unit = {
    // fmap (g . f) = fmap g . fmap f
    assertEquals(
      Functor.fmapMaybe(Functor.g compose Functor.f)(Just(10)),
      (Functor.fmapMaybe(Functor.g) compose Functor.fmapMaybe(Functor.f))(Just(10))
    )
    assertEquals(
      Functor.fmapMaybe(Functor.id compose Functor.f)(Nothing),
      (Functor.fmapMaybe(Functor.id) compose Functor.fmapMaybe(Functor.f))(Nothing)
    )
  }
}

class FunctorConstTest {
  @Test
  def singularityTest(): Unit = {
    // fmap g = fmap f = fmap g = fmap (g . f) = fmap (id . g . f)= fmap id = id
    assertThat(
      Functor.id(Const(10)),   // Image of black hole - singularity
      allOf(
        equalTo(Functor.fmapConst(Functor.id)(Const(10))),
        equalTo(Functor.fmapConst(Functor.f)(Const(10))),
        equalTo(Functor.fmapConst(Functor.g)(Const(10))),
        equalTo(Functor.fmapConst(Functor.id compose Functor.g compose Functor.f)(Const(10))),
        equalTo(Functor.fmapConst(Functor.g compose Functor.f)(Const(10)))
      )

    )
  }

  @Test
  def preserveIdentityTest(): Unit = {
    // Preserve `identity`
    // fmap id = id Tip! fmap (id Fa) = id Fa
    assertEquals(
      Functor.fmapConst(Functor.id)(Const(10)),
      Functor.id(Const(10))
    )
  }

  @Test
  def preserveCompositionTest(): Unit = {
    // Preserves `composition`
    // fmap (f . id) = fmap f . fmap id
    assertEquals(
      Functor.fmapConst(Functor.id compose Functor.f)(Const(10)),
      (Functor.fmapConst(Functor.id) compose Functor.fmapConst(Functor.f))(Const(10))
    )

    // fmap (g . f) = fmap g . fmap f
    assertEquals(
      Functor.fmapConst(Functor.g compose  Functor.f)(Const(10)),
      (Functor.fmapConst(Functor.g) compose Functor.fmapConst(Functor.f))(Const(10))
    )
  }
}
