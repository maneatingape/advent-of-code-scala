package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite:
  val sample = Seq(
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#")

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) == 7)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) == 336)
  }
