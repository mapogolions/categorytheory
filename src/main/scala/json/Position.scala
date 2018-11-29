package io.github.mapogolions.json.adt


case class Pointer(val row: Int, val col: Int) {
  def incRow = Pointer(row + 1, 0)
  def incCol = Pointer(row, col + 1)
}

class State(val lines: List[String], val ptr: Pointer) {
  override def toString = s"State(${lines}, ${ptr})"
  def line = 
    if (ptr.row < lines.length) lines(ptr.row) 
    else "end of file"
  
  def char =
    if (ptr.row >= lines.length) (this, None)
    else if (ptr.col < line.length) 
      (State(lines, ptr.incCol), Some(line(ptr.col)))
    else (State(lines, ptr.incRow), Some('\n'))

  def readAll: List[Char] = char match {
    case (_, None) => Nil
    case (state, Some(ch)) => ch :: state.readAll
  }
}

object State {
  def apply(lines: List[String]=Nil, ptr: Pointer=Pointer(0, 0)) = 
    new State(lines, ptr)
  def unapply(st: State) = Some(st.lines, st.ptr)
  def from(source: String): State =
    if (!source.nonEmpty) State(Nil, Pointer(0, 0))
    else State(source.split("\n").toList, Pointer(0, 0))
}

class Position(val line: String, val row: Int, val col: Int) {
  override def toString = s"Position(${line}, ${row}, ${col})"
}

object Position {
  def apply(line: String, row: Int, col: Int) = new Position(line, row, col)
  def unapply(pos: Position) = Some(pos.line, pos.row, pos.col)
  def from(state: State) = Position(state.line, state.ptr.row, state.ptr.col)
}