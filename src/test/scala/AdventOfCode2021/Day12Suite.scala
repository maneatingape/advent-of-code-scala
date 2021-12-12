package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample = Seq(
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end"
  )

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample) == 10)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day12.part2(sample) == 36)
  }
