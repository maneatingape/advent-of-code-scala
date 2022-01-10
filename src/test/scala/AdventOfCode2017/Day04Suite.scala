package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day04Suite extends AnyFunSuite:
  val sample1 = Seq(
    "aa bb cc dd ee",
    "aa bb cc dd aa",
    "aa bb cc dd aaa")

  test("Part 1 should handle sample input correctly") {
    assert(Day04.part1(sample1) == 2)
  }

  val sample2 = Seq(
    "abcde fghij",
    "abcde xyz ecdab",
    "a ab abc abd abf abj",
    "iiii oiii ooii oooi oooo",
    "oiii ioii iioi iiio")

  test("Part 2 should handle sample input correctly") {
    assert(Day04.part2(sample2) == 3)
  }
