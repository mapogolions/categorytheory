package io.github.mapogolions.categorytheory


object Epimorphism {
  type a = String
  type b = Int
  type c = Boolean

  val g: a => b = x => x.toInt
  val f1: b => c = x => if (x <= 0) false else true
  val f2: b => c = x => if (x < 0) false else true

  def firstConditionSatisfied: Boolean = f1 != f2
  def secondConditionSatisfied: Boolean = f1(0) != f2(0)
  def thirdConditionSatisfied: Boolean =
    (f1 compose g)("0") != (f2 compose g)("0")
}
