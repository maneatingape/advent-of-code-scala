package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day20Suite extends AnyFunSuite:
  val sample = Seq(
    "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>",
    "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>")

  test("Part 1 should handle sample input correctly") {
    assert(Day20.part1(sample) == 0)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day20.part2(sample) == 2)
  }
