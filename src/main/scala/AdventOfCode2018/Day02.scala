package AdventOfCode2018

object Day02:
  def part1(input: Seq[String]): Int =
    val occurences = input.map(_.groupMapReduce(identity)(_ => 1)(_ + _).values.toSet)
    occurences.count(_.contains(2)) * occurences.count(_.contains(3))

  def part2(input: Seq[String]): String =
    input.combinations(2).map { case Seq(first, second) =>
      first.zip(second).collect { case (a, b) if a == b => a }.mkString
    }
    .filter(_.size == input.head.size - 1).next()

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
