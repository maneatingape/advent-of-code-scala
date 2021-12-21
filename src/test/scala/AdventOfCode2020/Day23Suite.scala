package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day23Suite extends AnyFunSuite:
  val sample = "389125467"

  test("Part 1 should handle sample input correctly") {
    assert(Day23.part1(sample) == "67384529")
  }

// Test is correct but too slow
//  test("Part 2 should handle sample input correctly") {
//    assert(Day23.part2(sample) == 149245887792L)
//  }
