package AdventOfCode2017

object Day12:
  def cliques(input: Seq[String]): Set[Set[Int]] = input
    .map(_.split("\\D+").map(_.toInt).toSet)
    .foldLeft(Set.empty[Set[Int]]) { (groups, direct) =>
      val (other, linked) = groups.partition(_.intersect(direct).size == 0)
      other + (linked.flatten ++ direct)
    }

  def part1(input: Seq[String]): Int = cliques(input).filter(_.contains(0)).flatten.size

  def part2(input: Seq[String]): Int = cliques(input).size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2017/Day12.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
