package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day20Suite extends AnyFunSuite:
  val sample = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

  test("Part 1 should handle sample input correctly") {
    assert(Day20.part1(sample) == 31)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day20.part2(sample, 10) == 39)
  }
