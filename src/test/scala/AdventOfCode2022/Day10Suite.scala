package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day10Suite extends AnyFunSuite:
  val sample = Seq("noop", "addx 3", "addx -5")

  test("Parsing should handle sample input correctly") {
    assert(Day10.parse(sample) == Seq(-1, 1, 1, 1, 4, 4))
  }

