package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day19Suite extends AnyFunSuite:
  val sample = Seq(
    "     |          ",
    "     |  +--+    ",
    "     A  |  C    ",
    " F---|----E|--+ ",
    "     |  |  |  D ",
    "     +B-+  +--+ ",
    "                ")

  test("Part 1 should handle sample input correctly") {
    assert(Day19.part1(sample) == "ABCDEF")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day19.part2(sample) == 38)
  }
