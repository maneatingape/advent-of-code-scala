package AdventOfCode2021

object Day12:
  type Cave = Map[String, List[String]]

  def parse(input: Seq[String]): Cave = input.foldLeft[Cave](Map().withDefaultValue(Nil)) { case (cave, line) =>
    val Array(start, end) = line.split("-")
    cave.updated(start, end :: cave(start)).updated(end, start :: cave(end))
  }

  def descend(cave: Cave)(rule: (String, List[String]) => Boolean): Int =
    def small(name: String): Boolean = name.head.isLower
    def helper(current: String, path: List[String]): Int = cave(current).foldLeft(0) { (total, next) =>
      if next == "end" then total + 1
      else if next == "start" || (small(next) && rule(next, path.filter(small))) then total
      else total + helper(next, next :: path)
    }
    helper("start", Nil)

  def part1(input: Seq[String]): Int = descend(parse(input))((next, path) => path.contains(next))

  def part2(input: Seq[String]): Int = descend(parse(input)) { (next, path) =>
    val occurrences = (next :: path).groupBy(identity).values.map(_.length)
    occurrences.exists(_ > 2) || occurrences.count(_ == 2) > 1
  }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
