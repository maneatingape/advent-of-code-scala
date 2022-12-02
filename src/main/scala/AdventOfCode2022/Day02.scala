package AdventOfCode2022

object Day02:
  def part1(input: Seq[String]): Int =
    val order = Seq("", "B X", "C Y", "A Z", "A X", "B Y", "C Z", "C X", "A Y", "B Z")
    input.map(order.indexOf).sum

  def part2(input: Seq[String]): Int =
    val order = Seq("", "B X", "C X", "A X", "A Y", "B Y", "C Y", "C Z", "A Z", "B Z")
    input.map(order.indexOf).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
