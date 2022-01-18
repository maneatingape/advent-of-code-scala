package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite:
  val sample1 = Seq("R5", "L5", "R5", "R3")

  test("Part 1 should handle sample input correctly") {
    assert(Day01.part1(sample1) == 12)
  }

  val sample2 = Seq("R8", "R4", "R4", "R8")

  test("Part 2 should handle sample input correctly") {
    assert(Day01.part2(sample2) == 4)
  }
