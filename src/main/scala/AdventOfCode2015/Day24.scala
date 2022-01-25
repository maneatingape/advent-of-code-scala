package AdventOfCode2015

object Day24:
  def balance(input: Seq[Long], groups: Int): Long =
    val limit = input.sum / groups
    Iterator.from(1).flatMap(input.combinations(_).filter(_.sum == limit).map(_.product)).next()

  def part1(input: Seq[Long]): Long = balance(input, 3)

  def part2(input: Seq[Long]): Long = balance(input, 4)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day24.txt").getLines().map(_.toLong).toSeq
    println(part1(data))
    println(part2(data))
