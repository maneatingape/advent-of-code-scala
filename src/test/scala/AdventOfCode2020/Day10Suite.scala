package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day10Suite extends AnyFunSuite:
  val sample = Seq(16, 10, 15, 5, 1, 11, 7, 19 ,6 ,12, 4)

  test("Part 1 should handle sample input correctly") {
    assert(Day10.part1(sample) == 35L)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day10.part2(sample) == 8L)
  }
