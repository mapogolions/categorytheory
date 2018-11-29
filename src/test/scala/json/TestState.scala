package io.github.mapogolions.json

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo

import io.github.mapogolions.json.adt._


class TestState {
  @Test
  def TestReadAll: Unit = {
    assertEquals(
      State from "a\ncd\na" readAll,
      List('a', '\n', 'c', 'd', '\n', 'a', '\n')
    )

    assertEquals(
      State from "ab" readAll,
      List('a', 'b', '\n')
    )

    assertEquals(
      State from "a" readAll,
      List('a', '\n')
    )
    assertEquals(
      State from "" readAll,
      Nil
    )
  }

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
        List("string 1", "string 2", "string 3"), 
        Pointer(0, 0).incRow
      ).line,
      "string 2"
    )
    assertEquals(
      State.from("string 1\nstring 2\nstring 3").line,
      "string 1"
    )

    assertEquals(
      State(Nil, Pointer(0, 0)).line,
      "end of file"
    )
  }

  @Test
  def TestPointer: Unit = {
    assertEquals(
      Pointer(0, 0).incCol,
      Pointer(0, 1)
    )
    assertEquals(
      Pointer(0, 0).incRow,
      Pointer(1, 0)
    )
  }
}