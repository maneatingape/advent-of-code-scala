package AdventOfCode2022

object Day13:
  def compare(left: String, right: String): Boolean =
    if left.head == right.head then compare(left.tail, right.tail)
    else (left.head, right.head) match
      case (']', _) => true
      case (_, ']') => false
      case ('[', b) => compare(left.tail, b + "]" + right.tail)
      case (a, '[') => compare(a + "]" + left.tail, right.tail)
      case (a, b) => a < b

  def parse(input: Seq[String]): Seq[String] =
    input.filter(_.nonEmpty).map(_.replace("10", "A"))

  def part1(input: Seq[String]): Int =
    parse(input).grouped(2).zipWithIndex
      .collect { case (Seq(left, right), index) if compare(left, right) => index + 1 }
      .sum

  def part2(input: Seq[String]): Int =
    val dividers = Seq("[[2]]", "[[6]]")
    val sorted = (parse(input) ++ dividers).sortWith(compare)
    dividers.map(sorted.indexOf(_) + 1).product

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day13.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
