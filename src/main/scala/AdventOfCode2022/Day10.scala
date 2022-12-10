package AdventOfCode2022

object Day10:
  def parse(input: Seq[String]): Seq[Int] =
    val (_, signal) = input.foldLeft((1, Seq(-1))) { case ((x, xs), instruction) =>
      instruction match
        case "noop" => (x, xs ++ Seq(x))
        case s"addx $delta" => (x + delta.toInt, xs ++ Seq(x, x))
    }
    signal

  def part1(input: Seq[String]): Int =
    val signal = parse(input)
    Seq(20, 60, 100, 140, 180, 220).map(cycle => cycle * signal(cycle)).sum

  def part2(input: Seq[String]): String =
    parse(input).tail.grouped(40).map { row =>
      row.zipWithIndex.map((x, col) => if (x - col).abs <= 1 then '#' else '.')
    }
    .map(_.mkString).mkString("\n")

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day10.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
