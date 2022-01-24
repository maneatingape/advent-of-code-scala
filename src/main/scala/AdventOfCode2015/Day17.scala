package AdventOfCode2015

object Day17:
  def knapsack(input: Seq[Int]): Iterator[Set[(Int, Int)]] =
    input.zipWithIndex.toSet.subsets.filter(_.toSeq.map(_._1).sum == 150)

  def part1(input: Seq[Int]): Int = knapsack(input).size

  def part2(input: Seq[Int]): Int =
    val occurrences = knapsack(input).toSeq.groupBy(_.size)
    occurrences(occurrences.keys.min).size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day17.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
