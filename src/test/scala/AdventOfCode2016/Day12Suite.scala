package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample = Seq(
    "cpy 41 a",
    "inc a",
    "inc a",
    "dec a",
    "jnz a 2",
    "dec a")

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample) == 42)
  }
