package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day24Suite extends AnyFunSuite:
  val sample = Seq(
    "0/2",
    "2/2",
    "2/3",
    "3/4",
    "3/5",
    "0/1",
    "10/1",
    "9/10")

  test("Part 1 should handle sample input correctly") {
    assert(Day24.part1(sample) == 31)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day24.part2(sample) == 19)
  }
