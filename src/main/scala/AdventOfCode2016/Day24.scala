package AdventOfCode2016

object Day24:
  val orthogonal = Seq((1, 0), (-1, 0), (0, 1), (0, -1))

  case class Point(x: Int, y: Int):
    def adjacent: Seq[Point] = orthogonal.map(delta)
    def delta(dx: Int, dy: Int) = Point(x + dx, y + dy)

  def parse(input: Seq[String]): (Set[Point], Map[Int, Point]) =
    val points = for y <- input.indices; x <- input.head.indices if input(y)(x) != '#' yield Point(x, y)
    val pois = for y <- input.indices; x <- input.head.indices if input(y)(x).isDigit yield input(y)(x).asDigit -> Point(x, y)
    (points.toSet, pois.toMap)

  def bfs(grid: Set[Point], start: Point, end: Point): Int =
    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.Queue(start)

    while todo.nonEmpty do
      val current = todo.dequeue()
      if current == end then return cost(current)
      val nextCost = cost(current) + 1

      current.adjacent.filter(grid.contains).foreach { next =>
        if !cost.contains(next) || nextCost < cost(next) then
          cost(next) = nextCost
          todo.enqueue(next)
      }
    end while

    -1
  end bfs

  def graph(input: Seq[String]): Map[Int, Map[Int, Int]] =
    val (grid, pois) = parse(input)
    pois.transform((_, start) => pois.transform((_, end) => bfs(grid, start, end)))

  def tsp(pairs: Map[Int, Map[Int, Int]], routes: Iterator[Seq[Int]]): Int =
    routes.map(_.sliding(2).map(next => pairs(next.head)(next.last)).sum).min

  def part1(input: Seq[String]): Int =
    val pairs = graph(input)
    val routes = (1 to pairs.keys.max).permutations.map(_.prepended(0))
    tsp(pairs, routes)

  def part2(input: Seq[String]): Int =
    val pairs = graph(input)
    val routes = (1 to pairs.keys.max).permutations.map(_.prepended(0).appended(0))
    tsp(pairs, routes)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day24.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
