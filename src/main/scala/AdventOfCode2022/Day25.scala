package AdventOfCode2022

object Day25:
  def fromSnafu(s: String): Long = s.foldLeft(0L) { (total, c) =>
    val digit = c match
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '=' => -2
      case '-' => -1
    5 * total + digit
  }

  def toSnafu(i: Long, s: String = ""): String = if i == 0 then s else
    val (digit, prefix) = i % 5 match
      case 0 => (0, "0")
      case 1 => (1, "1")
      case 2 => (2, "2")
      case 3 => (-2, "=")
      case 4 => (-1, "-")
    toSnafu((i - digit) / 5, prefix + s)

  def part1(input: Seq[String]): String = toSnafu(input.map(fromSnafu).sum)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day25.txt").getLines().toSeq
    println(part1(data))
