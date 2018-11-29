package io.github.mapogolions.json

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo

import io.github.mapogolions.json.adt._


class TestState {
  @Test
  def TestStateChar: Unit = {
    val s0 = State.from("one\ntwo")
    val s1 = s0.char
    assertEquals(s1._2, Some('o'))
  }

  @Test
  def TestStateLine: Unit = {
    assertEquals(
      State(
        Array("string 1", "string 2", "string 3"), 
        Position(0, 0).incRow
      ).line,
      "string 2"
    )
    assertEquals(
      State.from("string 1\nstring 2\nstring 3").line,
      "string 1"
    )

    assertEquals(
      State(Array.empty, Position(0, 0)).line,
      "end of file"
    )
  }

  @Test
  def TestPosition: Unit = {
    assertEquals(
      Position(0, 0).incCol,
      Position(0, 1)
    )
    assertEquals(
      Position(0, 0).incRow,
      Position(1, 0)
    )
  }
}