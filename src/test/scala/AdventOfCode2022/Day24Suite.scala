package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day24Suite extends AnyFunSuite:
  val sample = Seq(
    "#.######",
    "#>>.<^<#",
    "#.<..<<#",
    "#>v.><>#",
    "#<^v^^>#",
    "######.#")

  test("Part 1 should handle sample input correctly") {
    assert(Day24.part1(sample) == 18)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day24.part2(sample) == 54)
  }
