package AdventOfCode2017

object Day04:
  def part1(input: Seq[String]): Int = input.count { line =>
    val array = line.split(" ")
    array.size == array.toSet.size
  }

  def part2(input: Seq[String]): Int = input.count { line =>
    val array = line.split(" ").map(_.sorted)
    array.size == array.toSet.size
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day04.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
