package AdventOfCode2017

object Day13:
  def parse(input: Seq[String]): Seq[(Int, Int)] = input.map { line =>
    val Array(depth, range) = line.split(": ").map(_.toInt)
    depth -> range
  }

  def severity(firewall: Seq[(Int, Int)], delay: Int): Seq[Int] = firewall.flatMap { (depth, range) =>
    val caught = (depth + delay) % (2 * range - 2) == 0
    Option.when(caught)(depth * range)
  }

  def part1(input: Seq[String]): Int = severity(parse(input), 0).sum

  def part2(input: Seq[String]): Int =
    val firewall = parse(input)
    Iterator.from(0).indexWhere(severity(firewall, _).isEmpty)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day13.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
