package io.github.mapogolions.cats

import scala.language.implicitConversions
import io.github.mapogolions.json.ops.ParserOps
import io.github.mapogolions.json.adt._


object Main {
  import ParserOps._

  def main(args: Array[String]): Unit = {
    println("category theory")
    val chain = (('a' parse )
      |> { _ toInt }
      |> { _ toDouble }
      |> { _ toString}
      apply "another"
    )
    // println(chain)
    val memo = (('a' parse) 
      >> ('b' parse) 
      >> ('c' parse) 
      >> ('d' parse)
      >> ('e' parse)
      |> { _ toInt }
      |> { _ + 1 }
      |> { _ toChar })      
    println(memo apply "abcdeoogle")
  }
  def msg = "I was compiled by dotty :)"
}