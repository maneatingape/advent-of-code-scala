package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample1 = Seq(
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    "mem[8] = 11",
    "mem[7] = 101",
    "mem[8] = 0")

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample1) == 165)
  }

  val sample2 = Seq(
    "mask = 000000000000000000000000000000X1001X",
    "mem[42] = 100",
    "mask = 00000000000000000000000000000000X0XX",
    "mem[26] = 1")

  test("Part 2 should handle sample input correctly") {
    assert(Day14.part2(sample2) == 208)
  }
