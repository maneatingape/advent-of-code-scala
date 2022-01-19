package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  val sample1= "X(8x2)(3x3)ABCY"

  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(sample1) == 18)
  }

  val sample2 = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"

  test("Part 2 should handle sample input correctly") {
    assert(Day09.part2(sample2) == 445)
  }
