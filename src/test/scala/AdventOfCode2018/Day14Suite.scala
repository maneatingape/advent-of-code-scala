package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample = 9

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample) == "5158916779")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day14.part2(sample) == 13)
  }
