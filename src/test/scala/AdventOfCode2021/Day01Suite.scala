package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite:
  val sample = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  test("Part 1 should handle sample input correctly") {
    assert(Day01.part1(sample) == 7)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day01.part2(sample) == 5)
  }
