package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample = "abc"

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample) == 22728)
  }

  // Part 2 is too slow