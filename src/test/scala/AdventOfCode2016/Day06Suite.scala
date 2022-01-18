package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day06Suite extends AnyFunSuite:
  val sample = Seq(
    "eedadn",
    "drvtee",
    "eandsr",
    "raavrd",
    "atevrs",
    "tsrnev",
    "sdttsa",
    "rasrtv",
    "nssdts",
    "ntnada",
    "svetve",
    "tesnvt",
    "vntsnd",
    "vrdear",
    "dvrsen",
    "enarar")

  test("Part 1 should handle sample input correctly") {
    assert(Day06.part1(sample) == "easter")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day06.part2(sample) == "advent")
  }
