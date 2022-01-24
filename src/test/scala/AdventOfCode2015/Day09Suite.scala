package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  val sample = Seq(
    "London to Dublin = 464",
    "London to Belfast = 518",
    "Dublin to Belfast = 141")

  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(sample) == 605)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day09.part2(sample) == 982)
  }
