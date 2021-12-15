package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day17Suite extends AnyFunSuite:
  val sample = Seq(
    ".#.",
    "..#",
    "###")

  test("Part 1 should handle sample input correctly") {
    assert(Day17.part1(sample) == 112)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day17.part2(sample) == 848)
  }
