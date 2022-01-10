package AdventOfCode2017

object Day01:
  def part1(input: Seq[Int]): Int = input
    .appended(input.head)
    .sliding(2)
    .map { case Seq(a, b) => if a == b then a else 0 }
    .sum

  def part2(input: Seq[Int]): Int = input
    .zipWithIndex
    .map { (d, i) => if d == input((i + input.size / 2) % input.size) then d else 0 }
    .sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day01.txt").mkString.trim.map(_.asDigit)
    println(part1(data))
    println(part2(data))
