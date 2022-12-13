package AdventOfCode2022

object Day13:
  def compare(left: String, right: String): Boolean = {
    if (left.nonEmpty && right.nonEmpty) {
      if (left.head != right.head) {
        (left.head, right.head) match
          case (']', _) => true
          case (_, ']') => false
          case ('[', b) => compare(left, s"[$b]" ++ right.tail)
          case (a, '[') => compare(s"[$a]" ++ left.tail, right)
          case (a, b) => a < b
      }
      else compare(left.tail, right.tail)
    }
    else left.nonEmpty
  }

  def parse(input: Seq[String]): Seq[String] =
    input.filter(_.nonEmpty).map(_.replace("10", "A"))

  def part1(input: Seq[String]): Int =
    parse(input).grouped(2).zipWithIndex.collect {
      case (Seq(left, right), index) if compare(left, right) => index + 1
    }
    .sum

  def part2(input: Seq[String]): Int =
    val divider1 = "[[2]]"
    val divider2 = "[[6]]"
    val sorted = (parse(input) ++ Seq(divider1, divider2)).sortWith(compare)
    (sorted.indexOf(divider1) + 1) * (sorted.indexOf(divider2) + 1)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day13.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
