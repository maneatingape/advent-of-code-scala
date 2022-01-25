package AdventOfCode2015

object Day20:
  def part1(target: Int): Int =
    val limit = target / 10
    val houses = Array.fill(limit + 1)(10)
    for x <- 2 to limit; y <- x to limit by x do houses(y) += 10 * x
    houses.indexWhere(_ >= target)

  def part2(target: Int): Int =
    val limit = target / 10
    val houses = Array.fill(limit + 1)(10)
    for x <- 2 to limit; y <- (x to limit by x).take(50) do houses(y) += 11 * x
    houses.indexWhere(_ >= target)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2015/Day20.txt").mkString.trim.toInt
    println(part1(data))
    println(part2(data))
