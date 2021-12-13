package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day11Suite extends AnyFunSuite:
  val sample = Seq(
    "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL")

  test("Part 1 should handle sample input correctly") {
    assert(Day11.part1(sample) == 37)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day11.part2(sample) == 26)
  }
