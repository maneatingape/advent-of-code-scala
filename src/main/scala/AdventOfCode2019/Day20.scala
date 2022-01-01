package AdventOfCode2019

object Day20:
  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

  case class State(path: Seq[(Portal, Int)], total: Int)

  sealed trait Portal
  case object Start extends Portal
  case object End extends Portal
  case class Outer(label: String) extends Portal
  case class Inner(label: String) extends Portal

  def parse(input: Seq[String]): Map[Portal, Set[(Portal, Int)]] =
    val (width, height) = (input.head.size, input.size)
    val maze = Seq.tabulate(width, height)((x, y) => Point(x, y) -> input(y)(x)).flatten.toMap
    val portals = findPortals(width - 3, height - 3, maze)

    val routes = for
      (start, portal) <- portals
    yield
      val cost = bfs(start, maze)
      val candidates = cost.keySet.intersect(portals.keySet) - start
      portal -> candidates.map(point => portals(point) -> cost(point))

    routes.map { (portal, routes) =>
      val linkedRoutes = routes.map {
        case (Outer(label), cost) => (Inner(label), cost + 1)
        case (Inner(label), cost) => (Outer(label), cost + 1)
        case other => other
      }
      portal -> linkedRoutes
    }
  end parse

  def findPortals(width: Int, height: Int, maze: Map[Point, Char]): Map[Point, Portal] =
    val patterns = Seq(Seq((-2, 0), (-1, 0)), Seq((1, 0), (2, 0)), Seq((0, -2), (0, -1)), Seq((0, 1), (0, 2)))

    val portals = for
      x <- 2 to width
      y <- 2 to height
      pattern <- patterns
    yield
      val point = Point(x, y)
      val Seq(first, second) = pattern.map(point.delta).map(maze)
      if maze(point) == '.' && first.isLetter && second.isLetter then
        val label = s"$first$second"
        val key =
          if label == "AA" then Start
          else if label == "ZZ" then End
          else if x == 2 || y == 2 || x == width || y == height then Outer(label)
          else Inner(label)
        Some(point -> key)
      else None

    portals.flatten.toMap
  end findPortals

  def bfs(start: Point, maze: Map[Point, Char]): Map[Point, Int] =
    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.Queue(start)
    val neighbours = Seq((-1, 0), (1, 0), (0, -1), (0, 1))

    while todo.nonEmpty do
      val point = todo.dequeue()
      neighbours
        .map(point.delta)
        .filter(next => maze(next) == '.')
        .filter(next => cost(point) + 1 < cost.getOrElse(next, Int.MaxValue))
        .foreach { next =>
          cost(next) = cost(point) + 1
          todo.enqueue(next)
        }

    cost.toMap
  end bfs

  def explore(input: Seq[String], recursive: Boolean): Int =
    val routes = parse(input)
    val todo = collection.mutable.Queue(State(Seq(Start -> 0), 0))
    var result = Int.MaxValue

    while todo.nonEmpty do
      val State(path, total) = todo.dequeue()
      val (current, depth) = path.last

      if total >= result then ()
      else if current == End then result = total
      else routes(current)
        .filter { (portal, cost) =>
          if !recursive then true
          else portal match
            case Inner(_) if depth == 0 => false
            case Start | End if depth > 0 => false
            case _ => true
        }
        .foreach { (portal, cost) =>
          val next = portal match
            case outer: Outer => (outer, depth + 1)
            case inner: Inner => (inner, depth - 1)
            case other => (other, depth)

          if !path.contains(next) then todo.enqueue(State(path.appended(next), total + cost))
        }
    end while

    result
  end explore

  def part1(input: Seq[String]): Int = explore(input, false)

  def part2(input: Seq[String]): Int = explore(input, true)

  def main(args: Array[String]): Unit = ()
    val data = io.Source.fromResource("AdventOfCode2019/Day20.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
