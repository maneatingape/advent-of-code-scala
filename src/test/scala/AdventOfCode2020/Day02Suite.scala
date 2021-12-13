package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample = Seq(
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc")

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample) == 2)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day02.part2(sample) == 1)
  }
