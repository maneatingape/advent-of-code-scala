package AdventOfCode2018

object Day06:
  case class Point(x: Int, y: Int):
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def orthogonal: Seq[Point] = Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(delta)

  def parse(input: Seq[String]): Seq[Point] = input.map(_.split(", ").map(_.toInt)).map(a => Point(a(0), a(1)))

  def part1(input: Seq[String]): Int =
    val coordinates = parse(input)

    val left = coordinates.map(_.x).min
    val right = coordinates.map(_.x).max
    val top = coordinates.map(_.y).min
    val bottom = coordinates.map(_.y).max
    val points = for x <- left to right; y <- top to bottom; yield Point(x, y)

    points.flatMap { point =>
      val Seq(first, second, rest*) = coordinates.sortBy(_.manhattan(point)): @unchecked
      val closest = first.manhattan(point) < second.manhattan(point)
      Option.when(closest)(first -> point)
    }
    .groupMap(_._1)(_._2)
    .filterNot((_, points) => points.exists(point => point.x == left || point.x == right || point.y == top || point.y == bottom))
    .values.map(_.size).max
  end part1

  def part2(input: Seq[String], threshold: Int): Int =
    val coordinates = parse(input)
    val meanX = coordinates.map(_.x).sum / coordinates.size
    val meanY = coordinates.map(_.x).sum / coordinates.size

    val start = Point(meanX, meanY)
    val todo = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(start)

    while todo.nonEmpty do todo.dequeue()
      .orthogonal
      .filterNot(todo.contains)
      .filterNot(visited.contains)
      .filter(next => coordinates.map(_.manhattan(next)).sum < threshold)
      .foreach { next =>
        todo.enqueue(next)
        visited += next
      }

    visited.size
  end part2

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day06.txt").getLines().toSeq
    println(part1(data))
    println(part2(data, 10000))
