package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day08Suite extends AnyFunSuite:
  val sample = Seq(
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6")

  test("Part 1 should handle sample input correctly") {
    assert(Day08.part1(sample) == 5)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day08.part2(sample) == 8)
  }
