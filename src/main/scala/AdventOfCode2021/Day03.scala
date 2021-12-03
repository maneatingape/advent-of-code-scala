package AdventOfCode2021

object Day03:
  def part1(input: Seq[String]): Int =
    val (_, gamma_rate, epsilon_rate) = input
      .map(_.toSeq)
      .transpose
      .map(s => s.count(_ == '1') > s.length / 2)
      .foldRight((1, 0, 0)) { case (isGamma, (power, gamma_rate, epsilon_rate)) =>
        if (isGamma) (power * 2, gamma_rate + power, epsilon_rate) else (power * 2, gamma_rate, epsilon_rate + power)
      }
    gamma_rate * epsilon_rate

  def part2(input: Seq[String]): Int =
    def findRating(predicate: (Int, Int) => Boolean) =
      (0 until input.head.length).foldLeft(input) { (seq, index) =>
        val count = seq.count(_.charAt(index) == '1')
        val threshold = (seq.length + 1) / 2
        val digit = if (predicate(count, threshold)) '1' else '0'
        if (seq.length > 1) seq.filter(_.charAt(index) == digit) else seq
      }

    def parseRating(predicate: (Int, Int) => Boolean) = java.lang.Integer.parseInt(findRating(predicate).head, 2)

    val generatorRating = parseRating(Ordering[Int].gteq)
    val scrubberRating = parseRating(Ordering[Int].lt)
    generatorRating * scrubberRating
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
