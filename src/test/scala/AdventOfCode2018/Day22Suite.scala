package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day22Suite extends AnyFunSuite:
  val sample = Seq("depth: 510", "target: 10,10")

  test("Part 1 should handle sample input correctly") {
    assert(Day22.part1(sample) == 114)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day22.part2(sample) == 45)
  }
