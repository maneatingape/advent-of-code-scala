package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day24Suite extends AnyFunSuite:
  val sample = Seq(
    "....#",
    "#..#.",
    "#..##",
    "..#..",
    "#....")

  test("Part 1 should handle sample input correctly") {
    assert(Day24.part1(sample) == 2129920)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day24.part2(sample, 10) == 99)
  }
