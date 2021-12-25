package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day25Suite extends AnyFunSuite:
  val sample = Seq(
    "v...>>.vv>",
    ".vv>>.vv..",
    ">>.>v>...v",
    ">>v>>.>.v.",
    "v>v.vv.v..",
    ">.>>..v...",
    ".vv..>.>v.",
    "v.v..>>v.v",
    "....v..v.>")

  test("Part 1 should handle sample input correctly") {
    assert(Day25.part1(sample) == 58)
  }
