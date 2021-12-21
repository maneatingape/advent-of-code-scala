package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day21Suite extends AnyFunSuite:
  val sample = Seq(
    "Player 1 starting position: 4",
    "Player 2 starting position: 8")

  test("Part 1 should handle sample input correctly") {
    assert(Day21.part1(sample) == 739785)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day21.part2(sample) == 444356092776315L)
  }
