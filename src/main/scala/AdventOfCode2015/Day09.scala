package AdventOfCode2015

object Day09:
  def parse(input: Seq[String]): Seq[(Set[String], Int)] = input.map { line =>
    val Array(first, _, second, _, distance) = line.split(" ")
    Set(first, second) -> distance.toInt
  }

  def tsp(input: Seq[String]): Iterator[Int] =
    val pairs = parse(input).toMap
    pairs.keys.flatten.toSeq.permutations.map(_.sliding(2).map(_.toSet).map(pairs).sum)

  def part1(input: Seq[String]): Int = tsp(input).min

  def part2(input: Seq[String]): Int = tsp(input).max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day09.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
