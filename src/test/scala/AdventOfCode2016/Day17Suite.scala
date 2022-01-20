package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day17Suite extends AnyFunSuite:
  val sample = "ihgpwlah"

  test("Part 1 should handle sample input correctly") {
    assert(Day17.part1(sample) == "DDRRRD")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day17.part2(sample) == 370)
  }
