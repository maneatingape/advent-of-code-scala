package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample = Seq(
    "<x=-1, y=0, z=2>",
    "<x=2, y=-10, z=-7>",
    "<x=4, y=-8, z=8>",
    "<x=3, y=5, z=-1>")

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample) == 183)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day12.part2(sample) == 2772)
  }
