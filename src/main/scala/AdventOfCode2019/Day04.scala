package AdventOfCode2019

object Day04:
  def part1(input: Seq[Int]): Int =
    (input.head to input.last).count { number =>
      def window = number.toString.map(_.asDigit).sliding(2)
      val rule1 = window.forall { case Seq(a, b) => a <= b }
      val rule2 = window.exists { case Seq(a, b) => a == b }
      rule1 && rule2
    }

  def part2(input: Seq[Int]): Int =
    (input.head to input.last).count { number =>
      def window(size: Int) = number.toString.map(_.asDigit).sliding(size)
      val rule1 = window(2).forall { case Seq(a, b) => a <= b }
      val rule2 = window(2).collect { case Seq(a, b) if a == b => a }.toSet
      val rule3 = window(3).collect { case Seq(a, b, c) if a == b && b == c => a }.toSet
      rule1 && (rule2 -- rule3).nonEmpty
    }

  def main(args: Array[String]): Unit =
    val data = Seq(172851, 675869)
    println(part1(data))
    println(part2(data))
