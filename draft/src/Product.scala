package io.github.mapogolions.categorytheory


object Product {
  type a = Int
  type b = Boolean
  type c = (a, b)

  val p: c => a = _ match {
    case (fst, _) => fst
  }
  val q: c => b = _ match {
    case (_, snd) => snd
  }

  // Applicant number 2: (Int, Int, Boolean)
  val p2: ((a, a, b)) => a = _ match {
    case (x, _, _) => x
  }
  val q2: ((a, a, b)) => b = _ match {
    case (_, _, x) => x
  }
  val m2: ((a, a, b)) => c = _ match {
    case (x, _, y) => (x, y)
  }
  val inverseM2: c => (a, a, b) = _ match {
    case (x, y) => (x, 0, y)
  }

  // Applicant number 1: Int
  val p1: a => a = identity _
  val q1: a => b = x => if (x == 0) false else true
  val m1: a => c = x => if (x == 0) (x, false) else (x, true)
  val inverseM1: c => a = _ match {
    case (x, _) => x
  }
}
