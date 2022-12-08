package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day08Suite extends AnyFunSuite:
  val sample = Seq(
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  )

  test("Part 1 should handle sample input correctly") {
    assert(Day08.part1(sample) == 21)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day08.part2(sample) == 8)
  }
