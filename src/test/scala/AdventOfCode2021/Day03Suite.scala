package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite:
  val sample = Seq("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) == 198)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) == 230)
  }
