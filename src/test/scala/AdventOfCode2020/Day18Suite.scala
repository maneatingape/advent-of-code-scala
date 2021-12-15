package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day18Suite extends AnyFunSuite:
  val sample = Seq(
    "1 + 2 * 3 + 4 * 5 + 6",
    "1 + (2 * 3) + (4 * (5 + 6))",
    "2 * 3 + (4 * 5)",
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

  test("Part 1 should handle sample input correctly") {
    assert(Day18.part1(sample) == 26457L)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day18.part2(sample) == 694173L)
  }
