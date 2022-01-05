package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day15Suite extends AnyFunSuite:
  val sample = Seq(
    "#######",
    "#.G...#",
    "#...EG#",
    "#.#.#G#",
    "#..G#E#",
    "#.....#",
    "#######")

  test("Part 1 should handle sample input correctly") {
    assert(Day15.part1(sample) == 27730)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day15.part2(sample) == 4988)
  }
