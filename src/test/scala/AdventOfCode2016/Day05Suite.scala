package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day05Suite extends AnyFunSuite:
  val sample = "abc"

  test("Part 1 should handle sample input correctly") {
    assert(Day05.part1(sample) == "18f47a30")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day05.part2(sample) == "05ace8e3")
  }
