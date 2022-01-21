package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day19Suite extends AnyFunSuite:
  val sample = 5

  test("Part 1 should handle sample input correctly") {
    assert(Day19.part1(sample) == 3)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day19.part2(sample) == 2)
  }
