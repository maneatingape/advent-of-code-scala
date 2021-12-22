package template

import org.scalatest.funsuite.AnyFunSuite

class Day00Suite extends AnyFunSuite:
  val sample = Seq()

  test("Part 1 should handle sample input correctly") {
    assert(Day00.part1(sample) == 123)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day00.part2(sample) == 456)
  }
