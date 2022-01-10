package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite:
  val sample1 = Seq(9,1,1,2,1,2,1,2,9)

  test("Part 1 should handle sample input correctly") {
    assert(Day01.part1(sample1) == 10)
  }

  val sample2 = Seq(1,2,3,4,2,5)

  test("Part 2 should handle sample input correctly") {
    assert(Day01.part2(sample2) == 4)
  }
