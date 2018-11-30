package io.github.mapogolions.cats

import scala.language.implicitConversions
import io.github.mapogolions.json.ops.ParserOps
import io.github.mapogolions.json.adt._
/* import io.github.mapogolions.json.functor.Functor
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._ */


object Main {
  import ParserOps._

  def main(args: Array[String]): Unit = {
    println("category theory")
    /* ('a'.moreOrEq(2) | "aabba" echo)
    ('a'.more(1) | "aabba" echo ) */
    /* ('a'.less(3) | "bla" echo)
    ('a'.lessOrEq(3) | "b" echo)*/
    // (tel1 | "3343-3434" echo)
    /* ('a'.less(3).count >> 'b'.once | "abb" echo)
    (email | "lalala@yandex.ru" echo) */
    ('a'.once |>> (x => x))  >> 'b'.once | "abba" echo
  }
  def msg = "I was compiled by dotty :)"
}
