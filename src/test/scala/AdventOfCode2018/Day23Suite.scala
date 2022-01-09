package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day23Suite extends AnyFunSuite:
  val sample1 = Seq(
    "pos=<0,0,0>, r=4",
    "pos=<1,0,0>, r=1",
    "pos=<4,0,0>, r=3",
    "pos=<0,2,0>, r=1",
    "pos=<0,5,0>, r=3",
    "pos=<0,0,3>, r=1",
    "pos=<1,1,1>, r=1",
    "pos=<1,1,2>, r=1",
    "pos=<1,3,1>, r=1")

  test("Part 1 should handle sample input correctly") {
    assert(Day23.part1(sample1) == 7)
  }

  val sample2 = Seq(
    "pos=<10,12,12>, r=2",
    "pos=<12,14,12>, r=2",
    "pos=<16,12,12>, r=4",
    "pos=<14,14,14>, r=6",
    "pos=<50,50,50>, r=200",
    "pos=<10,10,10>, r=5")

  test("Part 2 should handle sample input correctly") {
    assert(Day23.part2(sample2) == 36)
  }
