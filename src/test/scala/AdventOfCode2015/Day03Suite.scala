package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite:
  val sample = "^v^v^v^v^v"

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) == 2)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) == 11)
  }
