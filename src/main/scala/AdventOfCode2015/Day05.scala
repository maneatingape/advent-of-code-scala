package AdventOfCode2015

object Day05:
  def regex(patterns: String*): Seq[scala.util.matching.Regex] = patterns.map(_.r.unanchored)

  def part1(input: Seq[String]): Int =
    val Seq(vowels, pair, naughty) = regex("[aeiou].*[aeiou].*[aeiou]", "(.)\\1", "ab|cd|pq|xy")
    input.count(line => vowels.matches(line) && pair.matches(line) && !naughty.matches(line))

  def part2(input: Seq[String]): Int =
    val Seq(twoPair, triple) = regex("(..).*\\1", "(.).\\1")
    input.count(line => twoPair.matches(line) && triple.matches(line))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day05.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
