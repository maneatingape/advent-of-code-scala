package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day06Suite extends AnyFunSuite:
  val sample1 = Seq("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")

  test("Part 1 should handle sample input correctly") {
    assert(Day06.part1(sample1) == 42)
  }

  val sample2 = Seq("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN")

  test("Part 2 should handle sample input correctly") {
    assert(Day06.part2(sample2) == 4)
  }
