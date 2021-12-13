package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  val sample = Seq(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576).map(_.toLong)

  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(sample, 5) == 127)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day09.part2(sample, 5) == 62)
  }
