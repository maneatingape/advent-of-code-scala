package AdventOfCode2018

object Day22:
  sealed trait Tool
  case object Neither extends Tool
  case object Torch extends Tool
  case object ClimbingGear extends Tool

  sealed trait Terrain(val risk: Int, val allowedTools: Seq[Tool])
  case object Rocky extends Terrain(0, Seq(ClimbingGear, Torch))
  case object Wet extends Terrain(1, Seq(ClimbingGear, Neither))
  case object Narrow extends Terrain(2, Seq(Torch, Neither))

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def orthogonal: Seq[Point] = Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(delta)
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs

  case class State(location: Point, tool: Tool)

  def parse(input: Seq[String]): (Int, Point) =
    val Seq(Seq(depth), Seq(targetX, targetY)) = input.map(_.split("\\D+").filter(_.nonEmpty).toSeq)
    (depth.toInt, Point(targetX.toInt, targetY.toInt))

  def computeTerrain(depth: Int, target: Point): (Point => Terrain) =
    val terrain = Map(0 -> Rocky, 1 -> Wet, 2 -> Narrow)
    val cache = collection.mutable.Map[Point, Int]()
    def erosionLevel(point: Point): Int = cache.getOrElseUpdate(point, (geologicalIndex(point) + depth) % 20183)
    def geologicalIndex(point: Point) = point match
      case Point(0, 0) => 0
      case Point(x, 0) => 16807 * x
      case Point(0, y) => 48271 * y
      case point if point == target => 0
      case Point(x, y) => erosionLevel(Point(x - 1, y)) * erosionLevel(Point(x, y - 1))
    (point: Point) => terrain(erosionLevel(point) % 3)

  def neighbours(state: State, terrain: Point => Terrain): Map[State, Int] =
    val State(location, tool) = state

    val moveLocation = location.orthogonal
      .filter(next => next.x >= 0 && next.y >= 0)
      .filter(terrain(_).allowedTools.contains(state.tool))
      .map(State(_, tool) -> 1)
    val changeTool = terrain(location).allowedTools
      .filterNot(_ == tool)
      .map(State(location, _) -> 7)

    (moveLocation ++ changeTool).toMap
  end neighbours

  def aStar(target: Point, terrain: Point => Terrain): Int =
    val start = State(Point(0, 0), Torch)
    val end = State(target, Torch)

    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.PriorityQueue(start -> 0)(Ordering.by(-_._2))
    def heuristic(state: State) = state.location.manhattan(end.location)

    while todo.nonEmpty do
      val (current, wtf) = todo.dequeue()
      if current == end then return cost(end)

      neighbours(current, terrain).foreach { case (next, time) =>
        val nextCost = cost(current) + time
        if !cost.contains(next) || nextCost < cost(next) then
          cost(next) = nextCost
          val priority = nextCost + heuristic(next)
          todo.enqueue(next -> priority)
      }
    end while

    -1
  end aStar

  def part1(input: Seq[String]): Int =
    val (depth, target) = parse(input)
    val terrain = computeTerrain(depth, target)
    val risks = for x <- 0 to target.x; y <- 0 to target.y yield terrain(Point(x, y)).risk
    risks.sum

  def part2(input: Seq[String]): Int =
    val (depth, target) = parse(input)
    val terrain = computeTerrain(depth, target)
    aStar(target, terrain)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2018/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
