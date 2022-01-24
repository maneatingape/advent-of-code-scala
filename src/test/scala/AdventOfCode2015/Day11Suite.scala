package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day11Suite extends AnyFunSuite:
  val sample = "abcdefgh"

  test("Part 1 should handle sample input correctly") {
    assert(Day11.part1(sample) == "abcdffaa")
  }
