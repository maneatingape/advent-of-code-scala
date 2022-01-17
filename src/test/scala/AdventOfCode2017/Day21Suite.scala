package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day21Suite extends AnyFunSuite:
  val sample = Seq(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#")

  test("Part 1 should handle sample input correctly") {
    assert(Day21.fractal(sample, 2) == 12)
  }
