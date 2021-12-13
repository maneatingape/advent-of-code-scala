package AdventOfCode2020

object Day07:
  def parse(input: Seq[String]): Map[String, Seq[(Int, String)]] =
    val regex = "(\\d+) (.+) bags?".r
    input.map { line =>
      val Array(key, values) = line.split(" bags contain ")
      val bags = values.dropRight(1).split(", ").toSeq.collect {
        case regex(number, color) => number.toInt -> color
      }
      (key, bags)
    }
    .toMap

  def part1(input: Seq[String]): Int =
    val rules = parse(input).view.mapValues(_.map(_._2))
    def helper(bag: String): Boolean = rules(bag).contains("shiny gold") || rules(bag).exists(helper)
    rules.keys.count(helper)

  def part2(input: Seq[String]): Int =
    val rules = parse(input)
    def helper(bag: String): Int = 1 + rules(bag).map((number, bag) => number * helper(bag)).sum
    helper("shiny gold") - 1

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2020/Day07.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
