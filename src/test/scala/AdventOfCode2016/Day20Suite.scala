package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day20Suite extends AnyFunSuite:
  val sample = Seq("5-8", "0-2", "4-7")

  test("Part 1 should handle sample input correctly") {
    assert(Day20.part1(sample, 9) == 3)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day20.part2(sample, 9) == 2)
  }
