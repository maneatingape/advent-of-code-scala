package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day22Suite extends AnyFunSuite:
  val sample = Seq(
    "Player 1:", "9", "2", "6", "3", "1",
    "",
    "Player 2:", "5", "8", "4", "7", "10")

  test("Part 1 should handle sample input correctly") {
    assert(Day22.part1(sample) == 306)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day22.part2(sample) == 291)
  }
