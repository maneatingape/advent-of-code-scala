package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day22Suite extends AnyFunSuite:
  val sample = Seq(
    "        ...#",
    "        .#..",
    "        #...",
    "        ....",
    "...#.......#",
    "........#...",
    "..#....#....",
    "..........#.",
    "        ...#....",
    "        .....#..",
    "        .#......",
    "        ......#.",
    "",
    "10R5L5R10L4R5L5")

  test("Part 1 should handle sample input correctly") {
    assert(Day22.part1(sample) == 6032)
  }
