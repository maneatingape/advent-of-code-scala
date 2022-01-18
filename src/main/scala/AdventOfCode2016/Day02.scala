package AdventOfCode2016

object Day02:
  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

  def walk(input: Seq[String], keypad: Map[Point, Char]): String =
    val directions = Map('L' -> Point(-1, 0), 'R' -> Point(1, 0), 'U' -> Point(0, -1), 'D' -> Point(0, 1))
    input.scanLeft(Point(0, 0)) { (position, line) =>
      line.foldLeft(position) { (position, next) =>
        val candidate = position + directions(next)
        if keypad.contains(candidate) then candidate else position
      }
    }
    .tail.map(keypad).mkString

  def part1(input: Seq[String]): String =
    val keypad = Map(
      Point(-1, -1) -> '1', Point(0, -1) -> '2', Point(1, -1) -> '3',
      Point(-1, 0) ->  '4', Point(0, 0) ->  '5', Point(1, 0) ->  '6',
      Point(-1, 1) ->  '7', Point(0, 1) ->  '8', Point(1, 1) ->  '9')
    walk(input, keypad)

  def part2(input: Seq[String]): String =
    val keypad = Map(
      Point(2, -2) -> '1',
      Point(1, -1) -> '2', Point(2, -1) -> '3', Point(3, -1) -> '4',
      Point(0, 0) ->  '5', Point(1, 0) ->  '6', Point(2, 0) ->  '7', Point(3, 0) -> '8', Point(4, 0) -> '9',
      Point(1, 1) ->  'A', Point(2, 1) ->  'B', Point(3, 1) ->  'C',
      Point(2, 2) ->  'D')
    walk(input, keypad)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day02.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
