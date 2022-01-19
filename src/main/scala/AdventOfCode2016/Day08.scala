package AdventOfCode2016

object Day08:
  case class Point(x: Int, y: Int)

  def parse(input: Seq[String]): Set[Point] =
    val rect = "rect (\\d+)x(\\d+)".r
    val row = "rotate row y=(\\d+) by (\\d+)".r
    val col = "rotate column x=(\\d+) by (\\d+)".r
    input.foldLeft(Set.empty[Point]) { (points, next) =>
      next match
        case rect(width, height) =>
          points ++ (for x <- 0 until width.toInt; y <- 0 until height.toInt yield Point(x, y))
        case row(a, b) =>
          val (y, offset) = (a.toInt, b.toInt)
          points.map(p => if p.y != y then p else Point((p.x + offset) % 50, p.y))
        case col(a, b) =>
          val (x, offset) = (a.toInt, b.toInt)
          points.map(p => if p.x != x then p else Point(p.x, (p.y + offset) % 6))
    }

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day08.txt").getLines().toSeq
    val points = parse(data)
    println(points.size)
    for y <- 0 to 5 do
      println()
      for x <- 0 to 50 do
        print(if points.contains(Point(x, y)) then '#' else ' ')
