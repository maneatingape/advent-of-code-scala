package AdventOfCode2015

object Day02:
  def part1(input: Seq[String]): Int = input.map { line =>
    val Array(l, w, h) = line.split("x").map(_.toInt)
    val sides = Seq(l * w, w * h, h * l)
    2 * sides.sum + sides.min
  }
  .sum

  def part2(input: Seq[String]): Int = input.map { line =>
    val Array(l, w, h) = line.split("x").map(_.toInt)
    val sides = Seq(l + w, w + h, h + l)
    2 * sides.min + (l * w * h)
  }
  .sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
