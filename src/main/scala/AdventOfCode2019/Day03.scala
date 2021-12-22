package AdventOfCode2019

object Day03:
  case class Point(x: Int, y: Int):
    def updated(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def manhattan: Int = x.abs + y.abs

  def path(input: String): Seq[Point] = input.split(",")
    .foldLeft(Seq(Point(0, 0))) { (path, command) =>
      val delta = command.head match
        case 'L' => (-1, 0)
        case 'R' => (1, 0)
        case 'U' => (0, 1)
        case 'D' => (0, -1)
      path ++ Iterator.iterate(path.last)(_.updated.tupled(delta)).drop(1).take(command.tail.toInt).toSeq
    }

  def part1(input: Seq[String]): Int =
    val first = path(input(0))
    val second = path(input(1))
    val cross = first.tail.toSet.intersect(second.tail.toSet).toSeq
    cross.map(_.manhattan).min

  def part2(input: Seq[String]): Int =
    val first = path(input(0))
    val second = path(input(1))
    val cross = first.tail.toSet.intersect(second.tail.toSet).toSeq
    cross.map(point => first.indexOf(point) + second.indexOf(point)).min

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day03.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
