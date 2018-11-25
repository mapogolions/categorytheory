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
    println(parseDigit apply "0hello")
    println(parseLowerCase apply "stopper")
    println(parseUpperCase apply "stopper")
    println(parseThreeDigits apply "129stopper")
    println(parseThreeDigitsAsStr apply "129stoper")
    println(parseThreeDigitsAsStr apply "129stoper")
    println(parserThreeDigitsAsInt apply "129stoper")
  }
  def msg = "I was compiled by dotty :)"
}
