package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day07Suite extends AnyFunSuite:
  val sample = Seq(16,1,2,0,4,2,7,1,2,14)

  test("Part 1 should handle sample input correctly") {
    assert(Day07.part1(sample) == 37)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day07.part2(sample) == 168)
  }
