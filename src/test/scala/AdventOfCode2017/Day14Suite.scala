package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample = "flqrgnkx"

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample) == 8108)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day14.part2(sample) == 1242)
  }
