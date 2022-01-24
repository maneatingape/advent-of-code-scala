package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day19Suite extends AnyFunSuite:
  val sample1 = Seq(
    "H => HO",
    "H => OH",
    "O => HH",
    "",
    "HOH")

  test("Part 1 should handle sample input correctly") {
    assert(Day19.part1(sample1) == 4)
  }

  val sample2 = Seq(
    "e => H",
    "e => O",
    "H => HO",
    "H => OH",
    "O => HH",
    "",
    "HOHOHO")

  test("Part 2 should handle sample input correctly") {
    assert(Day19.part2(sample2) == 6)
  }
