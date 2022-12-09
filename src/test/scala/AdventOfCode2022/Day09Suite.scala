package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite:
  test("Part 1 should handle sample input correctly") {
    assert(Day09.part1(Seq("R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2")) == 13)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day09.part2(Seq("R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20")) == 36)
  }
