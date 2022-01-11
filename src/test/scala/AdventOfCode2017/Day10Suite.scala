package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day10Suite extends AnyFunSuite:
  val sample1 = "1,2,3"

  test("Part 1 should handle sample input correctly") {
    assert(Day10.part1(sample1) == 0)
  }

  val sample2 = "AoC 2017"

  test("Part 2 should handle sample input correctly") {
    assert(Day10.part2(sample2) == "33efeb34ea91902bb2f59c9920caa6cd")
  }
