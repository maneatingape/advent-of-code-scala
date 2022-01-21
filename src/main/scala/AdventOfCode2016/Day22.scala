package AdventOfCode2016

object Day22:
  val orthogonal = Seq((1, 0), (-1, 0), (0, 1), (0, -1))

  case class Point(x: Int, y: Int):
    def adjacent: Seq[Point] = orthogonal.map(delta)
    def delta(dx: Int, dy: Int) = Point(x + dx, y + dy)
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs

  case class Node(used: Int, avail: Int):
    def empty: Boolean = used == 0
    def fit(other: Node): Boolean = other.used <= avail
    def clear: Node = Node(0, avail + used)
    def merge(other: Node): Node = Node(used + other.used, avail - other.used)

  case class State(goal: Point, hole: Point, grid: Map[Point, Node]):
    def hash: (Point, Point) = (goal, hole)
    def finished: Boolean = goal == Point(0, 0)
    def heuristic: Int = 1000 * goal.manhattan(Point(0, 0)) + goal.manhattan(hole)

  def parse(input: Seq[String]): Map[Point, Node] = input.drop(2)
    .map { line =>
      val Array(x, y, _, used, avail ,_) = line.split("\\D+").tail.map(_.toInt)
      Point(x, y) -> Node(used, avail)
    }
    .toMap

  def swap(grid: Map[Point, Node], src: Point, dest: Point): Map[Point, Node] =
    grid.updated(src, grid(src).clear).updated(dest, grid(dest).merge(grid(src)))

  def initial(grid: Map[Point, Node]): Seq[State] =
    val goal = Point(grid.keys.map(_.x).max, 0)
    val pairs = grid.keys.toSeq.flatMap { src =>
      val node = grid(src)
      src.adjacent.filter(grid.contains).filter(grid(_).fit(node)).map(dest => src -> dest)
    }
    pairs.map((src, dest) => State(goal, src, swap(grid, src, dest)))

  def permutations(state: State): Seq[State] =
    val State(goal, hole, grid) = state
    hole.adjacent
      .filter(grid.contains)
      .filter(next => grid(hole).fit(grid(next)))
      .map { nextHole =>
        val nextGoal = if goal == nextHole then hole else goal
        State(nextGoal, nextHole, swap(grid, nextHole, hole))
      }

  def aStar(initial: Seq[State]): Int =
    val cost = collection.mutable.Map.from(initial.map(s => s.hash -> 1))
    val todo = collection.mutable.PriorityQueue.from(initial.map(s => s -> s.heuristic))(Ordering.by(-_._2))

    while todo.nonEmpty do
      val (current, _) = todo.dequeue()
      if current.finished then return cost(current.hash)
      val nextCost = cost(current.hash) + 1

      permutations(current).foreach { next =>
        if !cost.contains(next.hash) || nextCost < cost(next.hash) then
          cost(next.hash) = nextCost
          val priority = nextCost + next.heuristic
          todo.enqueue(next -> priority)
      }
    end while

    -1
  end aStar

  def part1(input: Seq[String]): Int = parse(input).toSeq.combinations(2).count { case Seq((_, first), (_, second)) =>
    (!first.empty && second.fit(first)) || (!second.empty && first.fit(second))
  }

  def part2(input: Seq[String]): Int = aStar(initial(parse(input)))

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2016/Day22.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
