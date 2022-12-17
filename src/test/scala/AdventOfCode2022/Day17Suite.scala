package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day17Suite extends AnyFunSuite:
  val sample = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

  test("Part 1 should handle sample input correctly") {
    assert(Day17.part1(sample) == 3068)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day17.part2(sample) == 1514285714288L)
  }
