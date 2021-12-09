package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  val sample = Seq(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678")

  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(sample) == 15)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day09.part2(sample) == 1134)
  }
