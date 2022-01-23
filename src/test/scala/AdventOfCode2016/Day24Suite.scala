package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day24Suite extends AnyFunSuite:
  val sample = Seq(
    "###########",
    "#0.1.....2#",
    "#.#######.#",
    "#4.......3#",
    "###########")

  test("Part 1 should handle sample input correctly") {
    assert(Day24.part1(sample) == 14)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day24.part2(sample) == 20)
  }
