package AdventOfCode2016

object Day09:
  val regex = """\((\d+)x(\d+)\)(.*)""".r

  def part1(input: String, size: Long = 0): Long = if input.isEmpty then size else input match
    case regex(count, repeat, rest) => part1(rest.drop(count.toInt), size + repeat.toLong * count.toInt)
    case _ => part1(input.tail, size + 1)

  def part2(input: String, size: Long = 0): Long = if input.isEmpty then size else input match
    case regex(count, repeat, rest) => part2(rest.drop(count.toInt), size + repeat.toLong * part2(rest.take(count.toInt)))
    case _ => part2(input.tail, size + 1)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day09.txt").mkString.trim
    println(part1(data))
    println(part2(data))
