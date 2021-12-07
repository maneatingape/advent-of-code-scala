package AdventOfCode2021

object Day06:
  def simulate(input: Seq[Int], days: Int): Long =
    val school = input.groupMapReduce(identity)(_ => 1L)(_ + _)
    (1 to days).foldLeft(school) { (current, _) =>
      val fish = current.map((timer, count) => (timer - 1, count)).withDefaultValue(0L)
      fish.removed(-1).updated(6, fish(-1) + fish(6)).updated(8, fish(-1))
    }
    .values.sum

  def part1(input: Seq[Int]): Long = simulate(input, 80)

  def part2(input: Seq[Int]): Long = simulate(input, 256)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day06.txt").mkString.trim.split(",").map(_.toInt).toSeq
    println(part1(data))
    println(part2(data))
