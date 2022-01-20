package AdventOfCode2016

object Day13:
  case class Point(x: Int, y: Int):
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def orthogonal: Seq[Point] = Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(delta)
    def inside: Boolean = x >= 0 && y >= 0
    def openSpace(favorite: Int): Boolean =
      val number = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + favorite
      Integer.bitCount(number) % 2 == 0
    def neighbours(favorite: Int) = orthogonal.filter(_.inside).filter(_.openSpace(favorite))

  def aStar(end: Point, favorite: Int): Int =
    val start = Point(1, 1)
    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.PriorityQueue(start -> 0)(Ordering.by(-_._2))
    def heuristic(point: Point) = point.manhattan(end)

    while todo.nonEmpty do
      val (current, _) = todo.dequeue()
      if current == end then return cost(end)
      val nextCost = cost(current) + 1

      current.neighbours(favorite).foreach { next =>
        if !cost.contains(next) || nextCost < cost(next) then
          cost(next) = nextCost
          val priority = nextCost + heuristic(next)
          todo.enqueue(next -> priority)
      }
    end while

    -1
  end aStar

  def bfs(threshold: Int, favorite: Int): Int =
    val start = Point(1, 1)
    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.Queue(start)

    while todo.nonEmpty do
      val current = todo.dequeue()
      val nextCost = cost(current) + 1

      if nextCost <= threshold then current.neighbours(favorite).foreach { next =>
        if !cost.contains(next) || nextCost < cost(next) then
          cost(next) = nextCost
          todo.enqueue(next)
      }
    end while

    cost.size
  end bfs

  def part1(input: Int): Int = aStar(Point(31, 39), input)

  def part2(input: Int): Int = bfs(50, input)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day13.txt").mkString.trim.toInt
    println(part1(data))
    println(part2(data))
