package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day13Suite extends AnyFunSuite:
  val sample = Seq(
    "0: 3",
    "1: 2",
    "4: 4",
    "6: 4")

  test("Part 1 should handle sample input correctly") {
    assert(Day13.part1(sample) == 24)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day13.part2(sample) == 10)
  }
