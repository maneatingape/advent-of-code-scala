package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample = Seq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50, 0)

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample) == 100)
  }
