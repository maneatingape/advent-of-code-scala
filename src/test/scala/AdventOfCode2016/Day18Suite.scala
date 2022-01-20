package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day18Suite extends AnyFunSuite:
  val sample = ".^^.^.^^^^"

  test("Part 1 should handle sample input correctly") {
    assert(Day18.safe(sample, 10) == 38)
  }
