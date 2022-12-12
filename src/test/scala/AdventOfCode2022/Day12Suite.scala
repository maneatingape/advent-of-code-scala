package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample = Seq(
    "Sabqponm",
    "abcryxxl",
    "accszExk",
    "acctuvwj",
    "abdefghi")

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample) == 31)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day12.part2(sample) == 29)
  }
