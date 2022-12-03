package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite:
  val sample = Seq(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) == 157)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) == 70)
  }
