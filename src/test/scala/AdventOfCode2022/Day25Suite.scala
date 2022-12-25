package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day25Suite extends AnyFunSuite:
  val sample = Seq(
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122")

  test("Part 1 should handle sample input correctly") {
    assert(Day25.part1(sample) == "2=-1=0")
  }
