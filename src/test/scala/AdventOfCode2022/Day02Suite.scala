package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample = Seq("A Y", "B X", "C Z")

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample) == 15)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day02.part2(sample) == 12)
  }
