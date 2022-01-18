package AdventOfCode2016

import org.scalatest.funsuite.AnyFunSuite

class Day04Suite extends AnyFunSuite:
  val sample = Seq(
    "aaaaa-bbb-z-y-x-123[abxyz]",
    "a-b-c-d-e-f-g-h-987[abcde]",
    "not-a-real-room-404[oarel]",
    "totally-real-room-200[decoy]")

  test("Part 1 should handle sample input correctly") {
    assert(Day04.part1(sample) == 1514)
  }
