package io.github.mapogolions.json.adt

import io.github.mapogolions.json.adt.{ Position, Source }


sealed trait Result[+A] { self =>
  def echo = println(self toString)
  def test(
    f: (elem: A, src: Source) => Unit, 
    g: (label: String, err: String, pos: Position) => Unit
  ) = self match {
    case Success(elem, src) => f(elem, src)
    case Failure(label, err, pos) => g(label, err, pos)
  }
}

case class Success[A](
  val elem: A, 
  val source: Source
) extends Result[A] {
  // override def toString = s"Success(${elem},${source})"
  override def toString = s"$elem"
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