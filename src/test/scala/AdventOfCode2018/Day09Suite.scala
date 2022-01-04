package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  val sample = "9 players; last marble is worth 25 points"

  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(sample) == 32)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day09.part2(sample) == 22563)
  }
