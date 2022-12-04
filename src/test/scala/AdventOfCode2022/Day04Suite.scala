package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day04Suite extends AnyFunSuite:
  val sample = Seq(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )

  test("Part 1 should handle sample input correctly") {
    assert(Day04.part1(sample) == 2)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day04.part2(sample) == 4)
  }
