package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day17Suite extends AnyFunSuite:
  val sample = (20, 30, -10, -5)

  test("Part 1 should handle sample input correctly") {
    assert(Day17.part1(sample) == 45)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day17.part2(sample) == 112)
  }
