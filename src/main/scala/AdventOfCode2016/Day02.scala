package AdventOfCode2016

object Day02:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

  def parse(input: String*): Map[Point, Char] =
    val points = for y <- input.indices; x <- input.indices if input(y)(x) != ' ' yield Point(x, y) -> input(y)(x)
    points.toMap

  def walk(input: Seq[String], start: Point, keypad: Map[Point, Char]): String =
    val directions = Map('L' -> Point(-1, 0), 'R' -> Point(1, 0), 'U' -> Point(0, -1), 'D' -> Point(0, 1))
    input.scanLeft(start) { (position, line) =>
      line.foldLeft(position) { (position, next) =>
        val candidate = position + directions(next)
        if keypad.contains(candidate) then candidate else position
      }
    }
    .tail.map(keypad).mkString

  def part1(input: Seq[String]): String =
    val keypad = parse("123", "456", "789")
    walk(input, Point(1, 1), keypad)

  def part2(input: Seq[String]): String =
    val keypad = parse("  1  ", " 234 ", "56789", " ABC ", "  D  ")
    walk(input, Point(0, 2), keypad)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
