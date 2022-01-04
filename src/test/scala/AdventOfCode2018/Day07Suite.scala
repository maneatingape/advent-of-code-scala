package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day07Suite extends AnyFunSuite:
  val sample = Seq(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.")

  test("Part 1 should handle sample input correctly") {
    assert(Day07.part1(sample) == "CABDFE")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day07.part2(sample, 2, 0) == 15)
  }
