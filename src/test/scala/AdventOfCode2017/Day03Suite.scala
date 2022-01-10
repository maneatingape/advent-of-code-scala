package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite:
  val sample = 12

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) == 3)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) == 23)
  }
