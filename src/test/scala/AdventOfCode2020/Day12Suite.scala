package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample = Seq(
    "F10",
    "N3",
    "F7",
    "R90",
    "F11")

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample) == 25)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day12.part2(sample) == 286)
  }
