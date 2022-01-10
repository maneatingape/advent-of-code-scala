package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day25Suite extends AnyFunSuite:
  val sample = Seq(
    "0,0,0,0",
    "15,0,0,0",
    "9,0,0,0",
    "3,0,0,0",
    "12,0,0,0",
    "6,0,0,0",
    "30,0,0,0",
    "33,0,0,0",
    "40,0,0,0")

  test("Part 1 should handle sample input correctly") {
    assert(Day25.part1(sample) == 3)
  }
