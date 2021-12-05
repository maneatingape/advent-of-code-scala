package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day05Suite extends AnyFunSuite:
  val sample = Seq(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2")

  test("Part 1 should handle sample input correctly") {
    assert(Day05.part1(sample) == 5)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day05.part2(sample) == 12)
  }
