package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day08Suite extends AnyFunSuite:
  val sample = Seq(
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10")

  test("Part 1 should handle sample input correctly") {
    assert(Day08.part1(sample) == 1)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day08.part2(sample) == 10)
  }
