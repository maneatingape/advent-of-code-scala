package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day18Suite extends AnyFunSuite:
  val sample = Seq(
    ".#.#...|#.",
    ".....#|##|",
    ".|..|...#.",
    "..|#.....#",
    "#.#|||#|#|",
    "...#.||...",
    ".|....|...",
    "||...#|.#|",
    "|.||||..|.",
    "...#.|..|.")

  test("Part 1 should handle sample input correctly") {
    assert(Day18.part1(sample) == 1147)
  }

  // Part 2 sample data has no cycle
