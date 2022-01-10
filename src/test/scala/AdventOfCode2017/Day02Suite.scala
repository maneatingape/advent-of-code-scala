package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample1 = Seq(
    "5 1 9 5",
    "7 5 3",
    "2 4 6 8")

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample1) == 18)
  }

  val sample2 = Seq(
    "5 9 2 8",
    "9 4 7 3",
    "3 8 6 5")

  test("Part 2 should handle sample input correctly") {
    assert(Day02.part2(sample2) == 9)
  }
