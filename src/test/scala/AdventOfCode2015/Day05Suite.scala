package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day05Suite extends AnyFunSuite:
  val sample1 = Seq(
    "ugknbfddgicrmopn",
    "aaa",
    "jchzalrnumimnmhp",
    "haegwjzuvuyypxyu",
    "dvszwmarrgswjxmb")

  test("Part 1 should handle sample input correctly") {
    assert(Day05.part1(sample1) == 2)
  }

  val sample2 = Seq(
    "qjhvhtzxzqqjkmpb",
    "xxyxx",
    "uurcxstgmygtbstg",
    "ieodomkazucvgmuy")

  test("Part 2 should handle sample input correctly") {
    assert(Day05.part2(sample2) == 2)
  }
