package AdventOfCode2020

object Day09:
  def part1(input: Seq[Long], preamble: Int): Long = input.sliding(preamble + 1)
    .map { window =>
      if window.dropRight(1).toSet.subsets(2).map(_.sum).contains(window.last) then 0 else window.last
    }
    .find(_ > 0).get

  def part2(input: Seq[Long], preamble: Int): Long =
    val target = part1(input, preamble)
    val state = Iterator
      .iterate((0, 2, input(0) + input(1))) { state =>
        val (start, end, total) = state
        if total < target then (start, end + 1, total + input(end))
        else if total > target then (start + 1, end, total - input(start))
        else state
      }
      .dropWhile(_._3 != target).next
    val slice = input.slice(state._1, state._2)
    slice.min + slice.max

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day09.txt").getLines().map(_.toLong).toSeq
    println(part1(data, 25))
    println(part2(data, 25))
