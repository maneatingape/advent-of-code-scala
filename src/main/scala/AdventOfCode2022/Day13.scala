package AdventOfCode2022

object Day13:
  def simplify(input: Seq[String]): Seq[String] = input.filter(_.nonEmpty).map(_.replace("10", "A"))

  def compare(left: String, right: String): Boolean = (left.head, right.head) match
    case (a, b) if a == b => compare(left.tail, right.tail)
    case (']', _) => true
    case (_, ']') => false
    case ('[', b) => compare(left.tail, s"$b]" + right.tail)
    case (a, '[') => compare(s"$a]" + left.tail, right.tail)
    case (a, b) => a < b

  def part1(input: Seq[String]): Int =
    val pairs = simplify(input).grouped(2).zipWithIndex
    pairs.collect { case (Seq(left, right), index) if compare(left, right) => index + 1 }.sum

  def part2(input: Seq[String]): Int =
    val dividers = Seq("[[2]]", "[[6]]")
    val sorted = (simplify(input) ++ dividers).sortWith(compare)
    dividers.map(sorted.indexOf(_) + 1).product

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2022/Day13.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
