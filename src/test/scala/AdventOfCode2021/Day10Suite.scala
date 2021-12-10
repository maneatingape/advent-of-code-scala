package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day10Suite extends AnyFunSuite:
  val sample = Seq(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]")

  test("Part 1 should handle sample input correctly") {
    assert(Day10.part1(sample) == 26397)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day10.part2(sample) == 288957)
  }
