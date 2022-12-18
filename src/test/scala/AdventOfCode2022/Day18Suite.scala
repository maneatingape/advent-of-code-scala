package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day18Suite extends AnyFunSuite:
  val sample = Seq(
    "2,2,2",
    "1,2,2",
    "3,2,2",
    "2,1,2",
    "2,3,2",
    "2,2,1",
    "2,2,3",
    "2,2,4",
    "2,2,6",
    "1,2,5",
    "3,2,5",
    "2,1,5",
    "2,3,5")

  test("Part 1 should handle sample input correctly") {
    assert(Day18.part1(sample) == 64)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day18.part2(sample) == 58)
  }
