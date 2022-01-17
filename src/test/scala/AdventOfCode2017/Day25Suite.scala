package AdventOfCode2017

import org.scalatest.funsuite.AnyFunSuite

class Day25Suite extends AnyFunSuite:
  val sample = Seq(
    "Begin in state A.",
    "Perform a diagnostic checksum after 6 steps.",
    "",
    "In state A:",
    "  If the current value is 0:",
    "    - Write the value 1.",
    "    - Move one slot to the right.",
    "    - Continue with state B.",
    "  If the current value is 1:",
    "    - Write the value 0.",
    "    - Move one slot to the left.",
    "    - Continue with state B.",
    "",
    "In state B:",
    "  If the current value is 0:",
    "    - Write the value 1.",
    "    - Move one slot to the left.",
    "    - Continue with state A.",
    "If the current value is 1:",
    "    - Write the value 1.",
    "    - Move one slot to the right.",
    "    - Continue with state A.")

  test("Part 1 should handle sample input correctly") {
    assert(Day25.part1(sample) == 3)
  }
