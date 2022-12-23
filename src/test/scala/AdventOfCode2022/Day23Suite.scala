package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day23Suite extends AnyFunSuite:
  val sample = Seq(
    "....#..",
    "..###.#",
    "#...#.#",
    ".#...##",
    "#.###..",
    "##.#.##",
    ".#..#..")

  test("Part 1 should handle sample input correctly") {
    assert(Day23.part1(sample) == 110)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day23.part2(sample) == 20)
  }
