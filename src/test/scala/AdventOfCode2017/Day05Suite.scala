package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day05Suite extends AnyFunSuite:
  val sample = Seq(0, 3, 0, 1, -3)

  test("Part 1 should handle sample input correctly") {
    assert(Day05.part1(sample) == 5)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day05.part2(sample) == 10)
  }
