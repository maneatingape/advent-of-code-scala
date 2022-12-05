package AdventOfCode2022

object Day05:
  def parse(input: Seq[String]): (Seq[String], Seq[(Int, Int, Int)]) =
    val (first, second) = input.splitAt(input.indexOf(""))

    val stacks = first.init.map { line =>
      (1 to line.length by 4).map(line)
    }
    .transpose
    .map(_.mkString.trim)

    val moves = second.tail.map { line =>
      val Array(amount, from, to) = line.split("\\D+").tail.map(_.toInt)
      (amount, from - 1, to - 1)
    }

    (stacks, moves)
  end parse

  def play(input: Seq[String], reverse: Boolean): String =
    val (stacks, moves) = parse(input)
    moves.foldLeft(stacks) { case (state, (amount, from, to)) =>
      val (prefix, suffix) = state(from).splitAt(amount)
      val result = if (reverse) prefix.reverse else prefix
      state.updated(from, suffix).updated(to, result + state(to))
    }
    .map(_.head).mkString

  def part1(input: Seq[String]): String = play(input, true)

  def part2(input: Seq[String]): String = play(input, false)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day05.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
