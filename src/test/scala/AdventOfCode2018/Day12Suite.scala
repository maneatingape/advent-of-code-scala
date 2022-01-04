package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite:
  val sample = Seq(
    "initial state: #..#.#..##......###...###",
    "",
    "...## => #",
    "..#.. => #",
    ".#... => #",
    ".#.#. => #",
    ".#.## => #",
    ".##.. => #",
    ".#### => #",
    "#.#.# => #",
    "#.### => #",
    "##.#. => #",
    "##.## => #",
    "###.. => #",
    "###.# => #",
    "####. => #")

  test("Part 1 should handle sample input correctly") {
    assert(Day12.part1(sample) == 325)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day12.part2(sample) == 50000000501L)
  }
