package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample = Seq(
    "498,4 -> 498,6 -> 496,6",
    "503,4 -> 502,4 -> 502,9 -> 494,9")

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample) == 24)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day14.part2(sample) == 93)
  }
