package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day22Suite extends AnyFunSuite:
  val sample = Seq(
    "deal into new stack",
    "cut -2",
    "deal with increment 7",
    "cut 8",
    "cut -4",
    "deal with increment 7",
    "cut 3",
    "deal with increment 9",
    "deal with increment 3",
    "cut -1")

  test("Part 1 should handle sample input correctly") {
    assert(Day22.part1(sample) == 1219L)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day22.part2(sample) == 117607927195067L)
  }
