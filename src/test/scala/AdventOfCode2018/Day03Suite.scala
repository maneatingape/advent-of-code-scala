package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite:
  val sample = Seq(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2")

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) == 4)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) == 3)
  }
