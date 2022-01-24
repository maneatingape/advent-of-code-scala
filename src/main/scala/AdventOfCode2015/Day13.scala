package AdventOfCode2015

object Day13:
  def parse(input: Seq[String]): Map[Seq[String], Int] =
    input.map { line =>
      val tokens = line.init.split(" ")
      Seq(tokens(0), tokens(10)) -> (if tokens(2) == "gain" then tokens(3).toInt else -tokens(3).toInt)
    }
    .toMap.withDefaultValue(0)

  def happiness(pairs: Map[Seq[String], Int], extra: Seq[String]): Int =
    val guests = pairs.keys.flatten.toSeq.distinct ++ extra
    guests.permutations.map { next =>
      next.appended(next.head).sliding(2).map(pair => pairs(pair) + pairs(pair.reverse)).sum
    }
    .max

  def part1(input: Seq[String]): Int = happiness(parse(input), Seq())

  def part2(input: Seq[String]): Int = happiness(parse(input), Seq("me"))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day13.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
