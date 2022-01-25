package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day24Suite extends AnyFunSuite:
  val sample = Seq(1L, 2L, 3L, 4L, 5L, 7L, 8L, 9L, 10L, 11L)

  test("Part 1 should handle sample input correctly") {
    assert(Day24.part1(sample) == 99)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day24.part2(sample) == 44)
  }
