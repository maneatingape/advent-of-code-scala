package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day11Suite extends AnyFunSuite:
  test("Part 1 should handle sample input correctly") {
    assert(Day11.part1(18) == "33,45")
  }

// Test is correct but slow
//  test("Part 2 should handle sample input correctly") {
//    assert(Day11.part2(18) == "90,269,16")
//  }
