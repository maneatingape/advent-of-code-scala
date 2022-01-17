package AdventOfCode2017

object Day24:
  case class Component(a: Int, b: Int, strength: Int):
    def matches(n: Int): Boolean = n == a || n == b
    def passthrough: Boolean = a == b
    def opposite(n: Int): Int = if n == a then b else a

  def parse(input: Seq[String]): Set[Component] = input.toSet.map { line =>
    val Array(a, b) = line.split("/").map(_.toInt)
    Component(a, b, a + b)
  }

  def build(input: Seq[String], ordering: Ordering[(Int, Int)]): Int =
    def helper(components: Set[Component], current: Int, depth: Int, total: Int): (Int, Int) =
      val candidates = components.filter(_.matches(current))
      val shortlist = candidates.find(_.passthrough).map(Set(_)).getOrElse(candidates)
      if shortlist.isEmpty then depth -> total
      else shortlist.map(next => helper(components - next, next.opposite(current), depth + 1, total + next.strength)).max(ordering)

    helper(parse(input), 0, 0, 0)._2
  end build

  def part1(input: Seq[String]): Int = build(input, Ordering.by(identity))

  def part2(input: Seq[String]): Int = build(input, Ordering.by(_._2))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
