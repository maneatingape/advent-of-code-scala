package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample = Seq("ULL", "RRDDD", "LURDL", "UUUUD")

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample) == "1985")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day02.part2(sample) == "5DB3")
  }
