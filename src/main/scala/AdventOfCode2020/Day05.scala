package AdventOfCode2020

object Day05:
  def seatId(line: String): Int =
    val binary = line.map(c => if c == 'B' || c == 'R' then 1 else 0)
    Integer.parseInt(binary.mkString, 2)

  def part1(input: Seq[String]): Int = input.map(seatId).max

  def part2(input: Seq[String]): Int =
    val ids = input.map(seatId)
    val missing = (ids.min to ids.max).toSet -- ids
    missing.head

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day05.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
