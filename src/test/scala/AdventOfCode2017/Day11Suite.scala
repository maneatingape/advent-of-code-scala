package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day11Suite extends AnyFunSuite:
  val sample = "se,sw,se,sw,sw,s,n"

  test("Part 1 should handle sample input correctly") {
    assert(Day11.part1(sample) == 3)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day11.part2(sample) == 4)
  }
