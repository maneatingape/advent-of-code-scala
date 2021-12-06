package AdventOfCode2021

object Day06:
  def simulate(input: Seq[Int], days: Int): Long = (1 to days)
    .foldLeft(input.groupMapReduce(identity)(_ => 1L)(_ + _)) { (previous, _) =>
      val next = previous.map((k, v) => (k - 1, v)).withDefaultValue(0L)
      next ++ Map(8 -> next(-1), 6 -> (next(6) + next(-1))) - (-1)
    }
    .values.sum

  def part1(input: Seq[Int]): Long = simulate(input, 80)

  def part2(input: Seq[Int]): Long = simulate(input, 256)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day06.txt").getLines().flatMap(_.split(",")).map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
