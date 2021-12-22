package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite:
  val sample = Seq("U7,R6,D4,L4", "R8,U5,L5,D3")

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) == 6)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) == 30)
  }
