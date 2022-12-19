package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day19Suite extends AnyFunSuite:
  val sample = Seq(
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.",
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

  test("Part 1 should handle sample input correctly") {
    assert(Day19.part1(sample) == 33)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day19.part2(sample) == 1157)
  }
