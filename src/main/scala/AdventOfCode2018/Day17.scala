package AdventOfCode2018

object Day17:
  case class Ground(top: Int, bottom: Int, clay: Set[Point])

  case class Point(x: Int, y: Int):
    def left: Point = Point(x - 1, y)
    def right: Point = Point(x + 1, y)
    def below: Point = Point(x, y + 1)

  sealed trait Result
  case object Stopped extends Result
  case object Moving extends Result

  def parse(input: Seq[String]): Ground =
    val vertical = """x=(\d+), y=(\d+)..(\d+)""".r
    val horizontal = """y=(\d+), x=(\d+)..(\d+)""".r
    val clay = input.toSet.flatMap {
      case vertical(x, start, end) => (start.toInt to end.toInt).map(y => Point(x.toInt, y))
      case horizontal(y, start, end) => (start.toInt to end.toInt).map(x => Point(x, y.toInt))
    }
    Ground(clay.map(_.y).min, clay.map(_.y).max, clay)

  def waterfall(ground: Ground): (Int, Int) =
    val stopped = collection.mutable.Set[Point]()
    val moving = collection.mutable.Set[Point]()

    def blocked(point: Point) = stopped.contains(point) || ground.clay.contains(point)

    def helper(point: Point): Result =
      if blocked(point) then Stopped
      else if point.y > ground.bottom || moving.contains(point) then Moving
      else helper(point.below) match
        case Moving =>
          moving += point
          Moving
        case Stopped =>
          val left = Iterator.iterate(point)(_.left).dropWhile(next => helper(next.below) == Stopped && !blocked(next.left)).next()
          val right = Iterator.iterate(point)(_.right).dropWhile(next => helper(next.below) == Stopped && !blocked(next.right)).next()
          val points = for x <- left.x to right.x yield Point(x, point.y)
          if blocked(left.left) && blocked(right.right) then
            stopped ++= points
            Stopped
          else
            moving ++= points
            Moving
    end helper

    helper(Point(500, 0))
    (moving.filterInPlace(point => point.y >= ground.top).size, stopped.size)
  end waterfall

  def part1(input: Seq[String]): Int =
    val (moving, stopped) = waterfall(parse(input))
    moving + stopped

  def part2(input: Seq[String]): Int =
    val (moving, stopped) = waterfall(parse(input))
    stopped

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day17.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
