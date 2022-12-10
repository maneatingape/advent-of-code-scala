package AdventOfCode2022

object Day10:
  def parse(input: Seq[String]): Seq[Int] =
    val (_, signal) = input.foldLeft((1, Seq(-1))) { case ((x, xs), instruction) =>
      instruction match
        case "noop" => (x, xs ++ Seq(x))
        case s"addx $value" => (x + value.toInt, xs ++ Seq(x, x))
    }
    signal

  def part1(input: Seq[String]): Unit =
    val signal = parse(input)
    println(Seq(20, 60, 100, 140, 180, 220).map(cycle => cycle * signal(cycle)).sum)

  def part2(input: Seq[String]): Unit =
    val signal = parse(input)
    for row <- 0 to 5 do
      println()
      for col <- 1 to 40 do
        val x = signal(40 * row + col)
        print(if (x - col + 1).abs <= 1 then '#' else ' ')

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day10.txt").getLines().toSeq
    part1(data)
    part2(data)
