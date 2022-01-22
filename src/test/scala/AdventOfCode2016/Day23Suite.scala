package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day23Suite extends AnyFunSuite:
  val sample = Seq(
    "cpy 2 a",
    "tgl a",
    "tgl a",
    "tgl a",
    "cpy 1 a",
    "dec a",
    "dec a")

  test("Part 1 should handle sample input correctly") {
    assert(Day23.part1(sample) == 3)
  }
