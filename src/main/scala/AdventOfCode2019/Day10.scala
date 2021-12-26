package AdventOfCode2019

object Day10:
  case class Point(x: Int, y: Int):
    def to(other: Point): Line = Line(y - other.y, other.x - x, x * other.y - other.x * y)

  case class Line(a: Int, b: Int, c: Int):
    def distance(p: Point): Int = a * p.x + b * p.y + c
    def normal(p: Point): Line = Line(b, -a, a * p.y - b * p.x)

  def parse(input: Seq[String]): Set[Point] =
    val height = input.size
    val width = input.head.size
    val points = for y <- 0 until height; x <- 0 until width if input(y)(x) == '#' yield Point(x, y)
    points.toSet

  def countVisible(points: Set[Point]): Set[(Point, Int)] =
    points.map { origin =>
      val visible = groupByLine(points, origin).values.map { points =>
        val distances = points.values
        distances.count(_ < 0).min(1) + distances.count(_ > 0).min(1)
      }
      origin -> visible.sum
    }

  def groupByLine(points: Set[Point], origin: Point): Map[Line, Map[Point, Int]] =
    (points - origin).foldLeft(Map.empty[Line, List[Point]]) { (lines, point) =>
      lines.keys.find(_.distance(point) == 0) match
        case Some(line) => lines.updated(line, point :: lines(line))
        case None => lines.updated(origin.to(point), List(point))
    }
    .map { (line, points) =>
      val normal = line.normal(origin)
      line -> points.zip(points.map(normal.distance)).toMap
    }

  def orderedClockwise(quadrant: Int, origin: Point, points: Set[Point]): Map[(Int, Int, Int), Point] =
    groupByLine(points, origin).flatMap { case (line, distances) =>
      val toLeft = points.count(point => line.distance(point) < 0)
      val inOrder = distances.values.toSeq.sorted.zipWithIndex.toMap
      distances.map((point, distance) => (inOrder(distance), quadrant, toLeft) -> point)
    }

  def part1(input: Seq[String]): Int =
    val points = parse(input)
    val (origin, visible) = countVisible(points).maxBy(_._2)
    visible

  def part2(input: Seq[String]): Int =
    val points = parse(input)
    val (origin, visible) = countVisible(points).maxBy(_._2)

    val topRight = orderedClockwise(0, origin, points.filter(p => p.x >= origin.x && p.y < origin.y))
    val bottomRight = orderedClockwise(1, origin, points.filter(p => p.x > origin.x && p.y >= origin.y))
    val bottomLeft = orderedClockwise(2, origin, points.filter(p => p.x <= origin.x && p.y > origin.y))
    val topLeft = orderedClockwise(3, origin, points.filter(p => p.x < origin.x && p.y <= origin.y))

    val all = topRight ++ bottomRight ++ bottomLeft ++ topLeft
    val key = all.keys.toSeq.sorted.apply(199)
    val result = all(key)

    100 * result.x + result.y
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day10.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
