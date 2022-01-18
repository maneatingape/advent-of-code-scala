package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day07Suite extends AnyFunSuite:
  val sample1 = Seq(
    "abba[mnop]qrst",
    "abcd[bddb]xyyx",
    "aaaa[qwer]tyui",
    "ioxxoj[asdfgh]zxcvbn")

  test("Part 1 should handle sample input correctly") {
    assert(Day07.part1(sample1) == 2)
  }

  val sample2 = Seq(
    "aba[bab]xyz",
    "xyx[xyx]xyx",
    "aaa[kek]eke",
    "zazbz[bzb]cdb")

  test("Part 2 should handle sample input correctly") {
    assert(Day07.part2(sample2) == 3)
  }
