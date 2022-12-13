package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day13Suite extends AnyFunSuite:
  val sample = Seq(
    "[1,1,3,1,1]",
    "[1,1,5,1,1]",
    "",
    "[[1],[2,3,4]]",
    "[[1],4]",
    "",
    "[9]",
    "[[8,7,6]]",
    "",
    "[[4,4],4,4]",
    "[[4,4],4,4,4]",
    "",
    "[7,7,7,7]",
    "[7,7,7]",
    "",
    "[]",
    "[3]",
    "",
    "[[[]]]",
    "[[]]",
    "",
    "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    "[1,[2,[3,[4,[5,6,0]]]],8,9]")

  test("Part 1 should handle sample input correctly") {
    assert(Day13.part1(sample) == 13)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day13.part2(sample) == 140)
  }
