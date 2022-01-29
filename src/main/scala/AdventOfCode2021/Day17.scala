package AdventOfCode2021

object Day17:
  def bruteForce(input: Seq[Int]): Seq[Int] =
    val Seq(left, right, bottom, top) = input

    def fire(dx : Int, dy: Int): Boolean = Iterator
      .iterate((0, 0, dx, dy))((x, y, dx, dy) => (x + dx, y + dy, (dx - 1).max(0), dy - 1))
      .takeWhile((x, y, _, _) => x <= right && y >= bottom)
      .exists((x, y, _, _) => x >= left && y <= top)

    for dx <- math.sqrt(left).toInt to right; dy <- bottom to -bottom if fire(dx, dy) yield (dy * (dy + 1)) / 2
  end bruteForce

  def parse(input: String): Seq[Int] = input.split("[^-\\d]+").tail.map(_.toInt)

  def part1(input: String): Int = bruteForce(parse(input)).max

  def part2(input: String): Int = bruteForce(parse(input)).size

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day17.txt").mkString.trim
    println(part1(data))
    println(part2(data))
