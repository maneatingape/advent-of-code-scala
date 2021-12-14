package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample = """
    NNCB

    CH -> B
    HH -> N
    CB -> H
    NH -> C
    HB -> C
    HC -> B
    HN -> C
    NN -> C
    BH -> H
    NC -> B
    NB -> B
    BN -> B
    BB -> N
    BC -> B
    CC -> N
    CN -> C
  """

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample) == 1588L)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day14.part2(sample) == 2188189693529L)
  }
