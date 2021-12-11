package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day11Suite extends AnyFunSuite:
  val sample = Seq(
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526")

  test("Part 1 should handle sample input correctly") {
    assert(Day11.part1(sample) == 1656)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day11.part2(sample) == 195)
  }
