package AdventOfCode2020

import org.scalatest.funsuite.AnyFunSuite

class Day21Suite extends AnyFunSuite:
  val sample = Seq(
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
    "trh fvjkl sbzzf mxmxvkd (contains dairy)",
    "sqjhc fvjkl (contains soy)",
    "sqjhc mxmxvkd sbzzf (contains fish)")

  test("Part 1 should handle sample input correctly") {
    assert(Day21.part1(sample) == 5)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day21.part2(sample) == "mxmxvkd,sqjhc,fvjkl")
  }
