package io.github.mapogolions.categorytheory


object Monomorphism {
  enum Banch { case One, Two, Three }
  type c = Banch
  type a = Int
  type b = Boolean

  val g: a => b = x => if (x == 0) false else true
  val f1: c => a = x =>  x match {
    case Banch.Three => 3
    case _ => x.enumTag
  }
  val f2: c => a = x => x match {
    case Banch.Three => 100
    case _ => x.enumTag
  }

  def firstConditionSatisfied: Boolean = f1 != f2
  def secondConditionSatisfied: Boolean = f1(Banch.Three) != f2(Banch.Three)
  def thirdConditionSatisfied: Boolean =
    (g compose f1)(Banch.Three) == (g compose f2)(Banch.Three)
}
