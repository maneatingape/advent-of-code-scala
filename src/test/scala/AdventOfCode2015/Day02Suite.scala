package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample = Seq("2x3x4", "1x1x10")

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample) == 101)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day02.part2(sample) == 48)
  }
