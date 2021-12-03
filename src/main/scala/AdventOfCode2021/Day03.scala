package AdventOfCode2021

object Day03:
  def part1(input: Seq[String]): Int =
    val (_, gammaRate, epsilonRate) = input
      .map(_.toSeq)
      .transpose
      .map(seq => seq.count(_ == '1') > seq.length / 2)
      .foldRight((1, 0, 0)) { case (isGamma, (power, gamma, epsilon)) =>
        if (isGamma) (power * 2, gamma + power, epsilon) else (power * 2, gamma, epsilon + power)
      }
    gammaRate * epsilonRate

  def part2(input: Seq[String]): Int =
    def findRating(seq: Seq[String], index: Int, predicate: (Int, Int) => Boolean): Int =
      if seq.length == 1 then
        Integer.parseInt(seq.head, 2)
      else
        val count = seq.count(_.charAt(index) == '1')
        val threshold = (seq.length + 1) / 2
        val digit = if (predicate(count, threshold)) '1' else '0'
        findRating(seq.filter(_.charAt(index) == digit), index + 1, predicate)

    val generatorRating = findRating(input, 0, Ordering[Int].gteq)
    val scrubberRating = findRating(input, 0, Ordering[Int].lt)
    generatorRating * scrubberRating
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
