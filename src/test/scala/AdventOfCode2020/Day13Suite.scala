package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day13Suite extends AnyFunSuite:
  val sample = Seq(
    "939",
    "7,13,x,x,59,x,31,19")

  test("Part 1 should handle sample input correctly") {
    assert(Day13.part1(sample) == 295L)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day13.part2(sample) == 1068781L)
  }
