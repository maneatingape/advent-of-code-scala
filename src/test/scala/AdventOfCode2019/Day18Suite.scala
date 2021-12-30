package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day18Suite extends AnyFunSuite:
  val sample1 = Seq(
    "########################",
    "#f.D.E.e.C.b.A.@.a.B.c.#",
    "######################.#",
    "#d.....................#",
    "########################")

  test("Part 1 should handle sample input correctly") {
    assert(Day18.part1(sample1) == 86)
  }

  val sample2 = Seq(
    "#############",
    "#g#f.D#..h#l#",
    "#F###e#E###.#",
    "#dCba@#@BcIJ#",
    "#############",
    "#nK.L@#@G...#",
    "#M###N#H###.#",
    "#o#m..#i#jk.#",
    "#############")

  test("Part 2 should handle sample input correctly") {
    assert(Day18.part1(sample2) == 72)
  }
