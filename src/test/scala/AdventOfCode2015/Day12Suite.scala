package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample1 = """{"a":{"b":4},"c":-1}"""

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample1) == 3)
  }

  val sample2 = """[1,{"c":"red","b":2},3]"""

  test("Part 2 should handle sample input correctly") {
    assert(Day12.part2(sample2) == 4)
  }
