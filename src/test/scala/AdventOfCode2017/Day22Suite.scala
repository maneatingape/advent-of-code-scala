package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day22Suite extends AnyFunSuite:
  val sample = Seq(
    "..#",
    "#..",
    "...")

  test("Part 1 should handle sample input correctly") {
    assert(Day22.part1(sample) == 5587)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day22.part2(sample) == 2511944)
  }
