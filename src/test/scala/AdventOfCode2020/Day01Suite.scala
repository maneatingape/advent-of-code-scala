package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite:
  val sample = Seq(1721, 979, 366, 299, 675, 1456)

  test("Part 1 should handle sample input correctly") {
    assert(Day01.part1(sample) == 514579)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day01.part2(sample) == 241861950)
  }
