package AdventOfCode2018

object Day10:
  case class Point(x: Int, y: Int, dx: Int, dy: Int):
    def next: Point = Point(x + dx, y + dy, dx, dy)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day10.txt").getLines().toSeq
    var points = data.map { line =>
      val Array(x, y, dx, dy) = line.split("[^-\\d]+").tail.map(_.toInt)
      Point(x, y, dx, dy)
    }

    var (left, right, top, bottom, seconds) = (0, 0, 0, 0, 0)
    var area = 550L

    while area >= 550L do
      points = points.map(_.next)
      left = points.map(_.x).min
      right = points.map(_.x).max
      top = points.map(_.y).min
      bottom = points.map(_.y).max
      area = (right - left).toLong * (bottom - top).toLong
      seconds += 1
    end while

    for y <- top to bottom do
      for x <- left to right do
        print(if points.exists(point => point.x == x && point.y == y) then '#' else ' ')
      println()
    println(s"Seconds: $seconds")
  end main
