package io.github.mapogolions.json.adt

import io.github.mapogolions.json.adt.{ Position, State }


sealed trait Result[+A] { self =>
  def echo = println(self toString)
}

case class Success[A](
  val elem: A, 
  val state: State
) extends Result[A] {
  override def toString = s"${elem}"
}

case class Failure(
  val label: String, 
  val err: String,
  val pos: Position
) extends Result[Nothing] {
  override def toString = {
    val where = s"Row: ${pos.row} Column: ${pos.col} "
    val what = s"Error parsing ${label}\n"
    val caret = s"${pos.line}\n${" " * (pos.col + 1)}^ ${err}"
    s"$where $what $caret"
  }
}