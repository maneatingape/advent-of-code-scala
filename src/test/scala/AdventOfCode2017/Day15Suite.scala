package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day15Suite extends AnyFunSuite:
  val sample = Seq(
    "Generator A starts with 65",
    "Generator B starts with 8921")

  test("Part 1 should handle sample input correctly") {
    assert(Day15.part1(sample) == 588)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day15.part2(sample) == 309)
  }
