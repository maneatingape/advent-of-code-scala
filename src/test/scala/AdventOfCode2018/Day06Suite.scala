package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day06Suite extends AnyFunSuite:
  val sample = Seq(
    "1, 1",
    "1, 6",
    "8, 3",
    "3, 4",
    "5, 5",
    "8, 9")

  test("Part 1 should handle sample input correctly") {
    assert(Day06.part1(sample) == 17)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day06.part2(sample, 32) == 16)
  }
