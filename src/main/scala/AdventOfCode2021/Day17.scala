package AdventOfCode2021

object Day17:
  def bruteForce(left : Int, right: Int, bottom: Int, top: Int): Seq[Int] =
    def fire(dx : Int, dy: Int): Boolean = Iterator
      .iterate((0, 0, dx, dy))((x, y, dx, dy) => (x + dx, y + dy, (dx - 1).max(0), dy - 1))
      .takeWhile((x, y, _, _) => x <= right && y >= bottom)
      .exists((x, y, _, _) => x >= left && x <= right && y >= bottom && y <= top)

    val minDx = Iterator.iterate(0)(_ + 1).dropWhile(x => x * (x + 1) / 2 < left).next()

    for dx <- minDx to right; dy <- bottom to -bottom if fire(dx, dy) yield (dy * (dy + 1)) / 2
  end bruteForce

  def part1(input: (Int, Int, Int, Int)): Int = bruteForce.tupled(input).max

  def part2(input: (Int, Int, Int, Int)): Int = bruteForce.tupled(input).size

  def main(args: Array[String]): Unit =
    val data = (124, 174, -123, -86)
    println(part1(data))
    println(part2(data))
