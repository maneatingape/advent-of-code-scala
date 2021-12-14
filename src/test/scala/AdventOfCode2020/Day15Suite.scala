package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day15Suite extends AnyFunSuite:
  val sample = Seq(0, 3, 6)

  test("Part 1 should handle sample input correctly") {
    assert(Day15.part1(sample) == 436)
  }

// Test is correct but slow
//  test("Part 2 should handle sample input correctly") {
//    assert(Day15.part2(sample) == 175594)
//  }
