package AdventOfCode2015

import org.scalatest.funsuite.AnyFunSuite

class Day08Suite extends AnyFunSuite:
  val sample = """
      ""
      "abc"
      "abc\"aaa"
      "\x27"
    """.split("\\s+").filter(_.nonEmpty).toSeq

  test("Part 1 should handle sample input correctly") {
    assert(Day08.part1(sample) == 12)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day08.part2(sample) == 19)
  }
