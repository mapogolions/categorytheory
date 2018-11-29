package io.github.mapogolions.cats

import scala.language.implicitConversions
import io.github.mapogolions.json.ops.ParserOps
import io.github.mapogolions.json.adt._
import io.github.mapogolions.json.functor.Functor
import io.github.mapogolions.json.functor.FunctorInstances._
import io.github.mapogolions.json.functor.FunctorSyntax._


object Main {
  import ParserOps._

  def main(args: Array[String]): Unit = {
    println("category theory")
    println('a'.parse >> 'c'.once | "ac")
    println("hello".parse | "hello world\nother")
  }
  def msg = "I was compiled by dotty :)"
}
