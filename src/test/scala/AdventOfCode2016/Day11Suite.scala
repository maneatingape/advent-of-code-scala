package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day11Suite extends AnyFunSuite:
  val sample = Seq(
    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.",
    "The second floor contains a hydrogen generator.",
    "The third floor contains a lithium generator.",
    "The fourth floor contains nothing relevant.")

  test("Part 1 should handle sample input correctly") {
    assert(Day11.part1(sample) == 11)
  }
