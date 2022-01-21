package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day21Suite extends AnyFunSuite:
  val sample = Seq(
    "swap position 4 with position 0",
    "swap letter d with letter b",
    "reverse positions 0 through 4",
    "rotate left 1 step",
    "move position 1 to position 4",
    "move position 3 to position 0",
    "rotate based on position of letter b",
    "rotate based on position of letter d")

  test("Part 1 should handle sample input correctly") {
    assert(Day21.part1(sample, "abcde") == "decab")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day21.part2(sample, "decab") == "abcde")
  }
