package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite:
  val sample = Seq("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

  test("Part 1 should handle sample input correctly") {
    assert(Day02.part1(sample) == 150)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day02.part2(sample) == 900)
  }
