package AdventOfCode2021

object Day12:
  type Cave = Map[String, List[String]]

  def small(name: String): Boolean = name != "start" && name.head.isLower

  def parse(input: Seq[String]): Cave =
    def helper(input: Seq[String], map: Cave): Cave = input match
      case Nil => map
      case head :: tail =>
        val Array(start, end) = head.split("-")
        val nextMap = map.updated(start, end :: map.getOrElse(start, Nil)).updated(end, start :: map.getOrElse(end, Nil))
        helper(tail, nextMap)
    helper(input, Map())

  def descend(cave: Cave, rule: (String, List[String]) => Boolean): Int =
    def helper(path: List[String]): Int =
      cave(path.head).map { next =>
        if next == "end" then 1
        else if next == "start" || (small(next) && rule(next, path.filter(small))) then 0
        else helper(next :: path)
      }
      .sum
    helper(List("start"))

  def part1(input: Seq[String]): Int = descend(parse(input), (next, path) => path.contains(next))

  def part2(input: Seq[String]): Int =
    def rule(next: String, path: List[String]): Boolean =
      val small = (next :: path)
      val occurrences = small.groupMapReduce(identity)(_ => 1)(_ + _)
      occurrences.exists((k, v) => v > 2) || occurrences.count((k, v) => v == 2) > 1
    descend(parse(input), rule)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
