package AdventOfCode2021

object Day07:
  def linearCost(x1: Int, x2: Int): Int = (x1 - x2).abs

  def triangleCost(x1: Int, x2: Int): Int = (linearCost(x1, x2) * (linearCost(x1, x2) + 1)) / 2

  def lowestCost(input: Seq[Int], cost: (Int, Int) => Int): Int = (input.min to input.max).map(x => input.map(cost(x, _)).sum).min

  def part1(input: Seq[Int]): Int = lowestCost(input, linearCost)

  def part2(input: Seq[Int]): Int = lowestCost(input, triangleCost)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day07.txt").mkString.trim.split(",").map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
