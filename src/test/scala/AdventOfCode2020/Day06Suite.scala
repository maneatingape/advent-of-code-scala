package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day06Suite extends AnyFunSuite:
  val sample = """
    abc

    a
    b
    c

    ab
    ac

    a
    a
    a
    a

    b
  """

  test("Part 1 should handle sample input correctly") {
    assert(Day06.part1(sample) == 11)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day06.part2(sample) == 6)
  }
