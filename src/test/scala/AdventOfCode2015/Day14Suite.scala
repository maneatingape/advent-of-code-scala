package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample = Seq(
    "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
    "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample, 1000) == 1120)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day14.part2(sample, 1000) == 689)
  }
