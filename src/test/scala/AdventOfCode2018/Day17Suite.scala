package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day17Suite extends AnyFunSuite:
  val sample = Seq(
    "x=495, y=2..7",
    "y=7, x=495..501",
    "x=501, y=3..7",
    "x=498, y=2..4",
    "x=506, y=1..2",
    "x=498, y=10..13",
    "x=504, y=10..13",
    "y=13, x=498..504")

  val day17 = Day17(sample)

  test("Part 1 should handle sample input correctly") {
    assert(day17.part1 == 57)
  }

  test("Part 2 should handle sample input correctly") {
    assert(day17.part2 == 29)
  }
