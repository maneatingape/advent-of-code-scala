package AdventOfCode2020

object Day01:
  def find(input: Seq[Int], size: Int): Int = input.toSet.subsets(size).find(_.sum == 2020).get.product

  def part1(input: Seq[Int]): Int = find(input, 2)

  def part2(input: Seq[Int]): Int = find(input, 3)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day01.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
