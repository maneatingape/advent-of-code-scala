package AdventOfCode2020

object Day05:
  def binary(seq: Seq[Int]): Int = Integer.parseInt(seq.mkString, 2)

  def seatId(line: String): Int =
    val (first, second) = line.splitAt(7)
    val row = first.map(c => if c == 'B' then 1 else 0)
    val column = second.map(c => if c == 'R' then 1 else 0)
    8 * binary(row) + binary(column)

  def part1(input: Seq[String]): Int = input.map(seatId).max

  def part2(input: Seq[String]): Int =
    val ids = input.map(seatId)
    val missing = (ids.min to ids.max).toSet -- ids
    missing.head

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day05.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
