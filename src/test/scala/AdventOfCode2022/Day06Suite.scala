package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day06Suite extends AnyFunSuite:
  test("Part 1 should handle sample input correctly") {
    assert(Day06.part1("bvwbjplbgvbhsrlpgdmjqwftvncz") == 5)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day06.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb") == 19)
  }
