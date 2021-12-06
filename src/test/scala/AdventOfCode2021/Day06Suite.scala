package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day06Suite extends AnyFunSuite:
  val sample = Seq(3,4,3,1,2)

  test("Part 1 should handle sample input correctly") {
    assert(Day06.part1(sample) == 5934L)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day06.part2(sample) == 26984457539L)
  }
