package AdventOfCode2021

object Day06:
  def simulate(input: Seq[Int], days: Int): Long =
    val fish = Array.tabulate(9)(i => input.count(_ == i).toLong)
    for day <- 0 until days do fish((day + 7) % 9) += fish(day % 9)
    fish.sum

  def part1(input: Seq[Int]): Long = simulate(input, 80)

  def part2(input: Seq[Int]): Long = simulate(input, 256)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day06.txt").mkString.trim.split(",").map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
