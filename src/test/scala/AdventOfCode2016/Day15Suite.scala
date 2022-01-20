package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day15Suite extends AnyFunSuite:
  val sample = Seq(
    "Disc #1 has 5 positions; at time=0, it is at position 4.",
    "Disc #2 has 2 positions; at time=0, it is at position 1.")

  test("Part 1 should handle sample input correctly") {
    assert(Day15.part1(sample) == 5)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day15.part2(sample) == 85)
  }
