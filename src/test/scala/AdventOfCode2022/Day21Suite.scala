package AdventOfCode2022

import org.scalatest.funsuite.AnyFunSuite

class Day21Suite extends AnyFunSuite:
  val sample = Seq(
    "root: pppw + sjmn",
    "dbpl: 5",
    "cczh: sllz + lgvd",
    "zczc: 2",
    "ptdq: humn - dvpt",
    "dvpt: 3",
    "lfqf: 4",
    "humn: 5",
    "ljgn: 2",
    "sjmn: drzm * dbpl",
    "sllz: 4",
    "pppw: cczh / lfqf",
    "lgvd: ljgn * ptdq",
    "drzm: hmdt - zczc",
    "hmdt: 32")

  test("Part 1 should handle sample input correctly") {
    assert(Day21.part1(sample) == 152)
  }
