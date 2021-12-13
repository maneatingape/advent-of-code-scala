package AdventOfCode2020

object Day02:
  val pattern = """(\d+)-(\d+) ([a-z]): ([a-z]+)""".r

  def part1(input: Seq[String]): Int = input.count { case pattern(lower, upper, letter, password) =>
    val occurrences = password.count(_ == letter.head)
    lower.toInt <= occurrences && occurrences <= upper.toInt
  }

  def part2(input: Seq[String]): Int = input.count { case pattern(lower, upper, letter, password) =>
    val first = password.charAt(lower.toInt - 1) == letter.head
    val second = password.charAt(upper.toInt - 1) == letter.head
    (first && !second) || (!first && second)
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
