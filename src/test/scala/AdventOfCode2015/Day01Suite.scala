package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite:
  val sample = "()())"

  test("Part 1 should handle sample input correctly") {
    assert(Day01.part1(sample) == -1)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day01.part2(sample) == 5)
  }
