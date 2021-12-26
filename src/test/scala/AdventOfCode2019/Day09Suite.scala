package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  val sample = Seq(1102,34915192,34915192,7,4,7,99,0).map(_.toLong)

  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(sample) == 1219070632396864L)
  }
