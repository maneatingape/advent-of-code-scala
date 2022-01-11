package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  val sample1 = "{{<a!>},{<a!>},{<a!>},{<ab>}}"

  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(sample1) == 3)
  }

  val sample2 = "<{o\"i!a,<{i<a>"

  test("Part 2 should handle sample input correctly") {
    assert(Day09.part2(sample2) == 10)
  }
