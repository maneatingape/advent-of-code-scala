package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day07Suite extends AnyFunSuite:
  val sample = Seq(
    "pbga (66)",
    "xhth (57)",
    "ebii (61)",
    "havc (66)",
    "ktlj (57)",
    "fwft (72) -> ktlj, cntj, xhth",
    "qoyq (66)",
    "padx (45) -> pbga, havc, qoyq",
    "tknk (41) -> ugml, padx, fwft",
    "jptl (61)",
    "ugml (68) -> gyxo, ebii, jptl",
    "gyxo (61)",
    "cntj (57)")

  test("Part 1 should handle sample input correctly") {
    assert(Day07.part1(sample) == "tknk")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day07.part2(sample) == 60)
  }
