package io.github.mapogolions.cats
import io.github.mapogolions.cats.functor.Functor
import io.github.mapogolions.cats.adt.{ List, Cons, Nil, Maybe, Just, Blank, Pair, Either, Left, Right }
import io.github.mapogolions.cats.monad.Monad
import io.github.mapogolions.cats.bifunctor.Bifunctor


object Main {
  import io.github.mapogolions.cats.functor.FunctorIntances._
  import io.github.mapogolions.cats.functor.FunctorSyntax._
  import io.github.mapogolions.cats.monad.MonadInstances._
  import io.github.mapogolions.cats.monad.MonadSyntax._
  import io.github.mapogolions.cats.bifunctor.BifunctorInstances._
  import io.github.mapogolions.cats.bifunctor.BifunctorSyntax._

  def main(args: Array[String]): Unit = {
    /* Monad test */

    /* List, Cons, Nil */
    println("***********MONAD TEST***********")
    println("\n List, Cons, Nil - test\n")
    println( List(1, 2, -2, 0, 10) ->> { a: Int => List(a > 0) })
    println( List(93, 45, 44) ->> { a: Int => List(a.toChar) } )
    println( Cons(1, Cons(0, Nil)) ->> { a: Int => Cons(a > 0, Nil) } )
    println( Nil ->> { a: Char => Cons(a.toChar, Nil) } )
    println( Cons("", Cons("hello", Nil)) ->> { a: String => Cons(a.length > 0, Nil)} )
    println( Cons('a', Nil) ->> { a: Char => Cons(a.toInt, Nil) } )
    println("\n\n")

    /*  Maybe, Just, Black */
    println("\n Maybe, Just, Black - test\n")
    println( Just(10) ->> { n => Just(n > 0) } )
    println( Just('w') ->> { ch => Just(ch + "orld")})
    println( Blank ->> { a: Int => Just( a > 0) } )
    println("\n\n")

  /* Fish operator  (f >=> g) */
    val f: String => List[(Char, String)] = msg => List((msg(0), msg.substring(1)))
    val g: ((Char, String)) => List[(Int, String)] = (ch, msg) => List((ch.toInt, msg))
    val h: ((Int, String)) => List[String] = (n, msg) => List((n + 1).toChar + msg)

    println(
      ({ a: Int => List(a > 0)} >=> { b: Boolean => List(b.toString)})(1)
    )
    println( (f >=> g >=> h)("Jotlin") ) // List(Kotlin)
    println( ({ n: Int => Just(n > 0) } >=> { flag: Boolean => Just(flag.toString) })(0) )



    println("\n\n*****************Test Bifunctor****************\n\n")
    println(
      Pair(10, 'M').lift({ _ > 0}, { _ + "ost" })()
    )
    println(Pair(10, 'M').bimap({ _ > 0}, { _ + "ost" }))
    println(Pair(-10, 30) umap { _ > 0 })
    println(Functor[List].map(List(1, 2, -1))(_ > 0))
    println( Functor[Maybe].map(Just(10))(_ > 0) )
    println()
    println( Functor[List].lift[Int, Boolean](_ > 0) )
    println()
    val listOpt = Functor[List] compose Functor[Maybe]
    println( listOpt.map(List(Just(0), Just(10), Just(20)))( _ + 1) )
    println()
    println( List(1, 2, 3) void )
    println( Just(10) as List(1, 2, 3))
    println( List(1, 2, 3) fproduct { _.toString })
    println()

    println( Bifunctor[Pair].bimap(Pair(100, false))({ _.toChar}, { !_ }) )
    println(Left(10).bimap({ _ > 10}, identity))
    println(Right(10).bimap(identity, { _ > 10}))
  }

  def msg = "I was compiled by dotty :)"

}
