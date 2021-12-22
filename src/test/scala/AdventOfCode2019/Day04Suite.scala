package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day04Suite extends AnyFunSuite:
  val sample = Seq(100000, 200000)

  test("Part 1 should handle sample input correctly") {
    assert(Day04.part1(sample) == 1231)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day04.part2(sample) == 898)
  }
