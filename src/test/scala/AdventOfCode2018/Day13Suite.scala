package AdventOfCode2018

import org.scalatest.funsuite.AnyFunSuite

class Day13Suite extends AnyFunSuite:
  val sample1 = Seq(
    """/->-\        """,
    """|   |  /----\""",
    """| /-+--+-\  |""",
    """| | |  | v  |""",
    """\-+-/  \-+--/""",
    """  \------/   """)

  test("Part 1 should handle sample input correctly") {
    assert(Day13.part1(sample1) == "7,3")
  }

  val sample2 = Seq(
    """/>-<\  """,
    """|   |  """,
    """| /<+-\""",
    """| | | v""",
    """\>+</ |""",
    """  |   ^""",
    """  \<->/""")

  test("Part 2 should handle sample input correctly") {
    assert(Day13.part2(sample2) == "6,4")
  }
