package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day05Suite extends AnyFunSuite:
  val sample = Seq(
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2")

  test("Part 1 should handle sample input correctly") {
    assert(Day05.part1(sample) == "CMZ")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day05.part2(sample) == "MCD")
  }
