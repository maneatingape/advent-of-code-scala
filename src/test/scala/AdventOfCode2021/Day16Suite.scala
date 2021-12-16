package AdventOfCode2021

import org.scalatest.funsuite.AnyFunSuite

class Day16Suite extends AnyFunSuite:
  val sample1 = Seq(
    "8A004A801A8002F478",
    "620080001611562C8802118E34",
    "C0015000016115A2E0802F182340",
    "A0016C880162017C3686B18A3D4780")

  test("Part 1 should handle sample input correctly") {
    assert(Day16.part1(sample1) == 82)
  }

  val sample2 = Seq(
    "C200B40A82",
    "04005AC33890",
    "880086C3E88112",
    "CE00C43D881120",
    "D8005AC2A8F0",
    "F600BC2D8F",
    "9C005AC2F8F0",
    "9C0141080250320F1802104A08")

  test("Part 2 should handle sample input correctly") {
    assert(Day16.part2(sample2) == 75)
  }
