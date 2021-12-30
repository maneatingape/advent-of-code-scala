package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day16Suite extends AnyFunSuite:
  val sample1 = "80871224585914546619083218645595"

  test("Part 1 should handle sample input correctly") {
    assert(Day16.part1(sample1) == "24176176")
  }

  val sample2 = "03036732577212944063491565474664"

  test("Part 2 should handle sample input correctly") {
    assert(Day16.part2(sample2) == "84462026")
  }
