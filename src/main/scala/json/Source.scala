package io.github.mapogolions.json.adt


case class Pointer(val row: Int, val col: Int) {
  def incRow = Pointer(row + 1, 0)
  def incCol = Pointer(row, col + 1)
}

class Source(val lines: Array[String], val ptr: Pointer) {
  override def toString = s"Source(${lines.mkString("")},${ptr})"
  def line = 
    if (ptr.row < lines.length) lines(ptr.row) 
    else "end of file"
  
  def char =
    if (ptr.row >= lines.length) (this, None)
    else if (ptr.col < line.length) 
      (Source(lines, ptr.incCol), Some(line(ptr.col)))
    else (Source(lines, ptr.incRow), Some('\n'))

  def readAll: List[Char] = char match {
    case (_, None) => Nil
    case (state, Some(ch)) => ch :: state.readAll
  }
}

object Source {
  def apply(lines: Array[String]=Array.empty, ptr: Pointer=Pointer(0, 0)) = 
    new Source(lines, ptr)
  def unapply(source: Source) = Some(source.lines, source.ptr)
  def from(text: String): Source =
    if (!text.nonEmpty) Source(Array.empty, Pointer(0, 0))
    else Source(text.split("\n"), Pointer(0, 0))
}

class Position(val line: String, val row: Int, val col: Int) {
  override def toString = s"Position(${line}, ${row}, ${col})"
}

object Position {
  def apply(line: String, row: Int, col: Int) = new Position(line, row, col)
  def unapply(pos: Position) = Some(pos.line, pos.row, pos.col)
  def from(source: Source) = Position(source.line, source.ptr.row, source.ptr.col)
}