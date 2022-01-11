package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample = Seq(
    "0 <-> 2",
    "1 <-> 1",
    "2 <-> 0, 3, 4",
    "3 <-> 2, 4",
    "4 <-> 2, 3, 6",
    "5 <-> 6",
    "6 <-> 4, 5")

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample) == 6)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day12.part2(sample) == 2)
  }
