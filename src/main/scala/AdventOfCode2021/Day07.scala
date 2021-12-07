package AdventOfCode2021

object Day07:
  def constantCost(x1: Int, x2: Int): Int = (x1 - x2).abs

  def increasingCost(x1: Int, x2: Int): Int = (constantCost(x1, x2) * (constantCost(x1, x2) + 1)) / 2

  def lowestCost(input: Seq[Int], cost: (Int, Int) => Int): Int = (input.min to input.max).map(x => input.map(cost(x, _)).sum).min

  def part1(input: Seq[Int]): Int = lowestCost(input, constantCost)

  def part2(input: Seq[Int]): Int = lowestCost(input, increasingCost)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day07.txt").getLines().flatMap(_.split(",")).map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
