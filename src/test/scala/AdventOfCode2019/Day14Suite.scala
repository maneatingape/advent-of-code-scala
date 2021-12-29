package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite:
  val sample = Seq(
    "157 ORE => 5 NZVS",
    "165 ORE => 6 DCFZ",
    "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
    "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
    "179 ORE => 7 PSHF",
    "177 ORE => 5 HKGWZ",
    "7 DCFZ, 7 PSHF => 2 XJWVT",
    "165 ORE => 2 GPVTF",
    "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

  test("Part 1 should handle sample input correctly") {
    assert(Day14.part1(sample) == 13312L)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day14.part2(sample) == 82892753L)
  }
