package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day13Suite extends AnyFunSuite:
  val sample = Seq(
    "6,10",
    "0,14",
    "9,10",
    "0,3",
    "10,4",
    "4,11",
    "6,0",
    "6,12",
    "4,1",
    "0,13",
    "10,12",
    "3,4",
    "3,0",
    "8,4",
    "1,10",
    "2,14",
    "8,10",
    "9,0",
    "",
    "fold along y=7",
    "fold along x=5")

  test("Part 1 should handle sample input correctly") {
    assert(Day13.part1(sample) == 17)
  }

  // Not possible to unit test part 2