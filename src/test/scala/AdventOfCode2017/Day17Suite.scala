package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day17Suite extends AnyFunSuite:
  val sample = 3

  test("Part 1 should handle sample input correctly") {
    assert(Day17.part1(3) == 638)
  }
