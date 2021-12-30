package AdventOfCode2019

object Day18:
  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def neighbours: Seq[Point] = Seq(delta(0, -1), delta(0, 1), delta(-1, 0), delta(1, 0))

  case class Move(from: Point, to: Point, cost: Int, neededKeys: Set[Char])

  extension (c: Char)
    def isRobot: Boolean = c == '@'
    def isWall: Boolean = c == '#'
    def isKey: Boolean = 'a' <= c && c <= 'z'
    def isDoor: Boolean = 'A' <= c && c <= 'Z'

  def parse(input: Seq[String]): (Map[Point, Char], Set[Point], Set[Point]) =
    val tunnels = for y <- input.indices; x <- input.head.indices yield Point(x, y) -> input(y)(x)
    val remainingKeys = tunnels.filter((_, letter) => letter.isKey).map(_._1)
    val robots = tunnels.filter((_, letter) =>  letter.isRobot).map(_._1)
    (tunnels.toMap, remainingKeys.toSet, robots.toSet)

  def dijkstra(tunnels: Map[Point, Char], start: Point): Seq[Move] =
    val cost = collection.mutable.Map(start -> 0)
    val keysNeeded = collection.mutable.Map(start -> Set.empty[Char])
    val todo = collection.mutable.PriorityQueue(start)(Ordering.by(cost))

    while todo.nonEmpty do
      val point = todo.dequeue()
      point.neighbours.view
        .filter(next => !tunnels(next).isWall)
        .filter(next => !cost.contains(next) || cost(point) + 1 < cost(next))
        .foreach { next =>
          cost(next) = cost(point) + 1
          keysNeeded(next) = if tunnels(next).isDoor then keysNeeded(point) + tunnels(next).toLower else keysNeeded(point)
          todo.enqueue(next)
        }

    cost.keys.map(point => Move(start, point, cost(point), keysNeeded(point))).toSeq
  end dijkstra

  def explore(input: Seq[String]): Int =
    val (tunnels, remainingKeys, robots) = parse(input)
    val cache = collection.mutable.Map[(Set[Point], Set[Char]), Int]()

    val routes = (remainingKeys ++ robots)
      .flatMap(point => dijkstra(tunnels, point))
      .map(move => (move.from, move.to) -> move)
      .toMap

    def helper(remainingKeys: Set[Point], robots: Set[Point], collectedKeys: Set[Char], total: Int, result: Int): Int =
      if total >= result || total >= cache.getOrElse((robots, collectedKeys), Int.MaxValue) then result
      else if remainingKeys.isEmpty then total
      else
        cache((robots, collectedKeys)) = total
        val candidates = for from <- robots; to <- remainingKeys yield routes.get((from, to))
        candidates.toSeq
          .flatten
          .filter(_.neededKeys.subsetOf(collectedKeys))
          .sortBy(_.cost)
          .foldLeft(result) { case (result, Move(from, to, cost, _)) =>
            helper(remainingKeys - to, robots - from + to, collectedKeys + tunnels(to), total + cost, result)
          }
      end if

    helper(remainingKeys, robots, Set(), 0, Int.MaxValue)
  end explore

  def part1(input: Seq[String]): Int = explore(input)

  def part2(input: Seq[String]): Int =
    val patched = input
      .updated(39, input(39).patch(39, "@#@", 3))
      .updated(40, input(40).patch(39, "###", 3))
      .updated(41, input(41).patch(39, "@#@", 3))
    explore(patched)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2019/Day18.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
