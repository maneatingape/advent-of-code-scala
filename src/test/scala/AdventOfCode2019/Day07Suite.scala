package AdventOfCode2019

import org.scalatest.funsuite.AnyFunSuite

class Day07Suite extends AnyFunSuite:
  val sample1 = Seq(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)

  test("Part 1 should handle sample input correctly") {
    assert(Day07.part1(sample1) == 65210)
  }

  val sample2 = Seq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)

  test("Part 2 should handle sample input correctly") {
    assert(Day07.part2(sample2) == 139629729)
  }
