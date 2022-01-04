package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day08Suite extends AnyFunSuite:
  val sample = Seq(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)

  test("Part 1 should handle sample input correctly") {
    assert(Day08.part1(sample) == 138)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day08.part2(sample) == 66)
  }
