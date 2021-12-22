package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day25Suite extends AnyFunSuite:
  val sample = Seq(5764801, 17807724)

  test("Part 1 should handle sample input correctly") {
    assert(Day25.part1(sample) == 14897079L)
  }
