package AdventOfCode2016

object Day10:
  def parse(input: Seq[String]): (Map[String, Set[Int]], Map[String, String], Map[String, String]) =
    val botRegex = "value (\\d+) goes to (.+)".r
    val bots = input.foldLeft(Map.empty[String, Set[Int]].withDefaultValue(Set())) { (bots, line) =>
      line match
        case botRegex(value, dest) => bots.updated(dest, bots(dest) + value.toInt)
        case _ => bots
    }
    val ruleRegex = "(.+) gives low to (.+) and high to (.+)".r
    val (low, high) = input.foldLeft((Map.empty[String, String], Map.empty[String, String])) { case ((low, high), line) =>
      line match
        case ruleRegex(bot, destLow, destHigh) => (low.updated(bot, destLow), high.updated(bot, destHigh))
        case _ => (low, high)
    }
    (bots, low, high)

  def zoomAround(input: Seq[String]): Map[String, Set[Int]] =
    val (bots, low, high) = parse(input)

    def helper(bots: Map[String, Set[Int]]): Map[String, Set[Int]] =
      bots.find((key, values) => key.startsWith("bot") && values.size == 2) match
        case None => bots
        case Some((key, values)) =>
          val next = bots.removed(key)
            .updated("done " + key, values)
            .updated(low(key), bots(low(key)) + values.min)
            .updated(high(key), bots(high(key)) + values.max)
          helper(next)

    helper(bots)
  end zoomAround

  def part1(input: Seq[String]): Int =
    zoomAround(input).find((_, values) => values == Set(17, 61)).get._1.filter(_.isDigit).toInt

  def part2(input: Seq[String]): Int =
    Seq("output 0", "output 1", "output 2").flatMap(zoomAround(input)).product

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day10.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
