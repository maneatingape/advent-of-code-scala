package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite:
  val sample = """
    |1000
    |2000
    |3000
    |
    |4000
    |
    |5000
    |6000
    |
    |7000
    |8000
    |9000
    |
    |10000
  """.stripMargin.trim

  test("Part 1 should handle sample input correctly") {
    assert(Day01.part1(sample) == 24000)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day01.part2(sample) == 45000)
  }
