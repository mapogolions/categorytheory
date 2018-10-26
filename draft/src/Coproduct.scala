package io.github.mapogolions.categorytheory


object Coproduct {
  type a = Int
  type b = Boolean
  type c = Int | Boolean

  val p: a => c = identity _
  val q: b => c = identity _
}
