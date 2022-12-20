package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day20Suite extends AnyFunSuite:
  val sample = Seq(1, 2 , -3, 3 , -2, 0, 4)

  test("Part 1 should handle sample input correctly") {
    assert(Day20.part1(sample) == 3)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day20.part2(sample) == 1623178306L)
  }
