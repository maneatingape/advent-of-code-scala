package AdventOfCode2020

object Day15:
  def play(input: Seq[Int], turn: Int): Int =
    val map = input.dropRight(1).zipWithIndex.toMap
    val (_, result) = (input.length until turn).foldLeft((map, input.last)) { case ((map, last), index) =>
      if map.contains(last) then
        val delta = index - 1 - map(last)
        (map.updated(last, index - 1), delta)
      else
        (map.updated(last, index - 1), 0)
    }
    result

  def part1(input: Seq[Int]): Int = play(input, 2020)

  def part2(input: Seq[Int]): Int = play(input, 30000000)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day15.txt").getLines().map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
