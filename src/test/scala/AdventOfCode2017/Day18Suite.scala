package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day18Suite extends AnyFunSuite:
  val sample1 = Seq(
    "set a 1",
    "add a 2",
    "mul a a",
    "mod a 5",
    "snd a",
    "set a 0",
    "rcv a",
    "jgz a -1",
    "set a 1",
    "jgz a -2 ")

  test("Part 1 should handle sample input correctly") {
    assert(Day18.part1(sample1) == 4)
  }

  val sample2 = Seq(
    "snd 1",
    "snd 2",
    "snd p",
    "rcv a",
    "rcv b",
    "rcv c",
    "rcv d")

  test("Part 2 should handle sample input correctly") {
    assert(Day18.part2(sample2) == 3)
  }
