package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample1 = Seq(
    "abcdef",
    "bababc",
    "abbcde",
    "abcccd",
    "aabcdd",
    "abcdee",
    "ababab")

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample1) == 12)
  }

  val sample2 = Seq(
    "abcde",
    "fghij",
    "klmno",
    "pqrst",
    "fguij",
    "axcye",
    "wvxyz")

  test("Part 2 should handle sample input correctly") {
    assert(Day02.part2(sample2) == "fgij")
  }
