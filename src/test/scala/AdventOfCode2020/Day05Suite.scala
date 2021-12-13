package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day05Suite extends AnyFunSuite:
  val sample = Seq("FBFBBFFLRR", "FBFBBFFRLR")

  test("Part 1 should handle sample input correctly") {
    assert(Day05.part1(sample) == 357)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day05.part2(sample) == 356)
  }
