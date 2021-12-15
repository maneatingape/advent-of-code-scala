package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day15Suite extends AnyFunSuite:
  val sample = Seq(
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581")

  test("Part 1 should handle sample input correctly") {
    assert(Day15.part1(sample) == 40)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day15.part2(sample) == 315)
  }
